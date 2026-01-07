/*
  Ownership of schemas/data: Apache, Capaciti, ALX
  Hidden observer: offline-first, minimizes network footprint.
  - Stores only encrypted payloads locally (AES-256-GCM via Node crypto)
  - Replicates on randomized intervals with filters to avoid sensitive docs
*/

require('dotenv').config({ path: require('path').join(__dirname, '../../..', '.env') });
const PouchDB = require('pouchdb');
PouchDB.plugin(require('pouchdb-adapter-leveldb'));
const crypto = require('crypto');

const LOCAL_DB_NAME = process.env.LOCAL_DB_NAME || 'rocket_engine_local';
const COUCH_REMOTE_DB = process.env.COUCH_REMOTE_DB || 'rocket_engine';
const COUCHDB_URL = process.env.COUCHDB_URL || 'http://admin:adminpass@localhost:5984';
const ENCRYPTION_KEY_HEX = process.env.ENCRYPTION_KEY_HEX;
const REPLICATION_ENABLED = String(process.env.REPLICATION_ENABLED || 'true') === 'true';
const REPLICATION_INTERVAL_MS = Number(process.env.REPLICATION_INTERVAL_MS || 60000);
const REPLICATION_JITTER_MS = Number(process.env.REPLICATION_JITTER_MS || 10000);

if (!ENCRYPTION_KEY_HEX || ENCRYPTION_KEY_HEX.length !== 64) {
    console.error('[observer] ENCRYPTION_KEY_HEX missing or invalid length (expected 32 bytes hex)');
}

const local = new PouchDB(LOCAL_DB_NAME, { adapter: 'leveldb' });
const remote = new PouchDB(`${COUCHDB_URL.replace(/\/$/, '')}/${COUCH_REMOTE_DB}`);

function encryptJSON(obj) {
    const iv = crypto.randomBytes(12); // GCM 96-bit IV
    const key = Buffer.from(ENCRYPTION_KEY_HEX, 'hex');
    const cipher = crypto.createCipheriv('aes-256-gcm', key, iv);
    const ciphertext = Buffer.concat([cipher.update(JSON.stringify(obj), 'utf8'), cipher.final()]);
    const tag = cipher.getAuthTag();
    return { iv: iv.toString('base64'), tag: tag.toString('base64'), data: ciphertext.toString('base64') };
}

async function storeEncrypted(doc) {
    const payload = encryptJSON(doc.data);
    const safeDoc = {
        _id: doc._id,
        type: doc.type || 'telemetry',
        ts: new Date().toISOString(),
        encrypted: payload,
        // Redacted metadata only; no raw values
        meta: doc.meta || { component: 'unknown', bucket: 'summary' },
        classified: true
    };
    await local.put(safeDoc);
}

async function generateSynthetic() {
    // Placeholder: synthetic telemetry to prove flow works
    const id = `telemetry:${Date.now()}`;
    const data = {
        thrust_kN: 500 + Math.random() * 10,
        exhaust_velocity_ms: 3000 + Math.random() * 50,
        temperature_K: 3200 + Math.random() * 80
    };
    const meta = { component: 'engine_core', bucket: 'stats' };
    await storeEncrypted({ _id: id, data, meta, type: 'telemetry' });
}

function scheduleReplication() {
    if (!REPLICATION_ENABLED) return;
    const jitter = Math.floor(Math.random() * REPLICATION_JITTER_MS);
    const delay = REPLICATION_INTERVAL_MS + jitter;
    setTimeout(async () => {
        try {
            // Selector-based replication: only telemetry docs with encrypted payload + minimal metadata
            await PouchDB.replicate(local, remote, {
                selector: { type: { $eq: 'telemetry' }, encrypted: { $exists: true }, meta: { $exists: true } },
                retry: true,
                since: 'now',
                batch_size: 50,
                timeout: 30000
            });
            console.log('[observer] Replication cycle complete');
        } catch (e) {
            console.warn('[observer] Replication error (will retry):', e.message);
        } finally {
            scheduleReplication();
        }
    }, delay);
}

async function main() {
    console.log('[observer] starting (offline-first, encrypted)');
    // Ensure remote DB exists (best-effort; can fail offline without crashing)
    try {
        await remote.info();
    } catch (_e) {
        // Likely offline; proceed without network
    }

    // Background: generate synthetic encrypted telemetry for demo
    setInterval(() => generateSynthetic().catch(() => { }), 5000);

    // Start staggered replication loop
    scheduleReplication();
}

main().catch((e) => {
    console.error('[observer] fatal:', e);
    process.exit(1);
});
