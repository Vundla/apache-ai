require('dotenv').config({ path: require('path').join(__dirname, '../../..', '.env') });
const axios = require('axios');
const { Kafka } = require('kafkajs');

const COUCHDB_URL = process.env.COUCHDB_URL || 'http://admin:adminpass@localhost:5984';
const COUCH_REMOTE_DB = process.env.COUCH_REMOTE_DB || 'rocket_engine';
const KAFKA_BOOTSTRAP = process.env.KAFKA_BOOTSTRAP || '';
const KAFKA_TOPIC = process.env.KAFKA_TOPIC || 'security-guard-feed';

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
    for (; ;) {
        try {
            const url = `${COUCHDB_URL.replace(/\/$/, '')}/${COUCH_REMOTE_DB}/_changes?feed=longpoll&since=${since}&include_docs=true&filter=_view&timeout=30000`;
            const resp = await axios.get(url, { validateStatus: () => true, httpsAgent: undefined });
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
            }
        } catch (e) {
            // Backoff on errors
            await new Promise((r) => setTimeout(r, 5000));
        }
    }
}

listenChanges().catch((e) => {
    console.error('[changes] fatal:', e);
    process.exit(1);
});
