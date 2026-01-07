import { reactions } from '../../../lib/catalog.js';

export default function handler(req, res) {
    if (req.method !== 'GET') {
        res.setHeader('Allow', ['GET']);
        return res.status(405).json({ error: 'method_not_allowed' });
    }

    res.status(200).json({ reactions });
}
