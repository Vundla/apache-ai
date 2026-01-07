import { findAction } from '../../../lib/catalog.js';

export default function handler(req, res) {
    if (req.method !== 'GET') {
        res.setHeader('Allow', ['GET']);
        return res.status(405).json({ error: 'method_not_allowed' });
    }

    const id = Number(req.query.id);
    const entry = findAction(id);
    if (!entry) {
        return res.status(404).json({ error: 'unknown_action', id });
    }

    res.status(200).json({ action: entry });
}
