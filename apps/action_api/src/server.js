import express from 'express';
import { actions, reactions, findAction, findReaction } from './catalog.js';

const app = express();
const PORT = process.env.PORT || 7070;
app.use(express.json());

app.get('/healthz', (_req, res) => {
    res.json({ status: 'ok', ts: Date.now(), apis: 21 });
});

app.get('/api/actions', (_req, res) => {
    res.json({ actions });
});

app.get('/api/actions/:id', (req, res) => {
    const id = Number(req.params.id);
    const entry = findAction(id);
    if (!entry) {
        return res.status(404).json({ error: 'unknown_action', id });
    }
    res.json({ action: entry, reactionEndpoint: entry.reactionEndpoint });
});

app.post('/api/actions/:id/trigger', (req, res) => {
    const id = Number(req.params.id);
    const entry = findAction(id);
    if (!entry) {
        return res.status(404).json({ error: 'unknown_action', id });
    }
    const payload = req.body ?? {};
    res.json({
        acknowledged: true,
        action: entry.action,
        routedReaction: entry.reactionEndpoint,
        payloadHash: payload.hash || null
    });
});

app.get('/api/reactions', (_req, res) => {
    res.json({ reactions });
});

app.get('/api/reactions/:id', (req, res) => {
    const id = Number(req.params.id);
    const entry = findReaction(id);
    if (!entry) {
        return res.status(404).json({ error: 'unknown_reaction', id });
    }
    res.json({ reaction: entry });
});

app.use((req, res) => {
    res.status(404).json({ error: 'route_not_found', path: req.path });
});

app.listen(PORT, () => {
    /* eslint-disable no-console */
    console.log(`[action-api] listening on ${PORT}`);
    /* eslint-enable no-console */
});
