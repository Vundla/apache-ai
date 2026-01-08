require('dotenv').config({ path: require('path').join(__dirname, '../../..', '.env') });
const axios = require('axios');
const fs = require('fs');
const https = require('https');
const { Kafka } = require('kafkajs');

const COUCHDB_URL = process.env.COUCHDB_URL || 'http://admin:adminpass@localhost:5984';
const COUCH_REMOTE_DB = process.env.COUCH_REMOTE_DB || 'rocket_engine';
const KAFKA_BOOTSTRAP = process.env.KAFKA_BOOTSTRAP || '';
const KAFKA_TOPIC = process.env.KAFKA_TOPIC || 'security-guard-feed';
const CHANGE_FEED_RETRY_MS = Number(process.env.CHANGE_FEED_RETRY_MS || 5000);
const CHANGE_FEED_MAX_ERRORS = Number(process.env.CHANGE_FEED_MAX_ERRORS || 5);
const COUCHDB_CA_FILE = process.env.COUCHDB_CA_FILE;
const COUCHDB_INSECURE_SKIP_VERIFY = String(process.env.COUCHDB_INSECURE_SKIP_VERIFY || 'false') === 'true';

const isHttps = COUCHDB_URL.startsWith('https://');
let httpsAgent;
if (isHttps) {
    const agentOptions = { rejectUnauthorized: !COUCHDB_INSECURE_SKIP_VERIFY };
    if (COUCHDB_CA_FILE) {
        try {
            agentOptions.ca = fs.readFileSync(COUCHDB_CA_FILE);
        } catch (err) {
            console.warn('[changes] unable to read CA file, continuing without it:', err.message);
        }
    }
    httpsAgent = new https.Agent(agentOptions);
}

async function startKafka() {
    if (!KAFKA_BOOTSTRAP) return null;
    const kafka = new Kafka({ clientId: 'change-feed-listener', brokers: KAFKA_BOOTSTRAP.split(',') });
    const producer = kafka.producer();
    await producer.connect();
    return producer;
}

async function listenChanges() {
    const producer = await startKafka().catch(() => null);
    console.log('[changes] listening to CouchDB _changes');
    let since = 'now';
    let consecutiveErrors = 0;
    for (; ;) {
        let shouldBackoff = false;
        try {
            const url = `${COUCHDB_URL.replace(/\/$/, '')}/${COUCH_REMOTE_DB}/_changes?feed=longpoll&since=${since}&include_docs=true&timeout=30000`;
            const resp = await axios.get(url, { validateStatus: () => true, httpsAgent });
            if (resp.status === 200 && resp.data && resp.data.results) {
                for (const change of resp.data.results) {
                    // Publish minimal metadata to Kafka if available
                    if (producer) {
                        const payload = {
                            id: change.id,
                            seq: change.seq,
                            deleted: !!change.deleted,
                            type: change.doc && change.doc.type,
                            ts: change.doc && change.doc.ts
                        };
                        await producer.send({ topic: KAFKA_TOPIC, messages: [{ value: JSON.stringify(payload) }] });
                    }
                }
                since = resp.data.last_seq || since;
                consecutiveErrors = 0;
            } else {
                consecutiveErrors += 1;
                console.warn('[changes] CouchDB response not OK', {
                    status: resp.status,
                    data: resp.data ? resp.data.error || resp.data.reason || resp.data : 'no-body',
                    attempts: consecutiveErrors
                });
            }
        } catch (e) {
            consecutiveErrors += 1;
            shouldBackoff = true;
            console.error('[changes] request error', { message: e.message, attempts: consecutiveErrors });
        }

        if (consecutiveErrors >= CHANGE_FEED_MAX_ERRORS) {
            console.error('[changes] max consecutive errors reached; exiting for supervisor restart');
            process.exit(1);
        }

        if (shouldBackoff || consecutiveErrors > 0) {
            await new Promise((r) => setTimeout(r, CHANGE_FEED_RETRY_MS));
        }
    }
}

listenChanges().catch((e) => {
    console.error('[changes] fatal:', e);
    process.exit(1);
});
