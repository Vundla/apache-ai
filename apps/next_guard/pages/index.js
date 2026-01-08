import Head from 'next/head';
import { useMemo, useState } from 'react';
import { actions, reactions, TOTAL_APIS } from '../lib/catalog';

const threatBands = ['Nominal', 'Watch', 'Alert', 'Lockdown'];

const signalColor = (score) => {
    if (score > 75) return '#ff8e53';
    if (score > 50) return '#ffa534';
    if (score > 25) return '#3dd598';
    return '#4d8dff';
};

export default function Home() {
    const [query, setQuery] = useState('');
    const [selectedId, setSelectedId] = useState(actions[0]?.id ?? 1);

    const filtered = useMemo(() => {
        return actions.filter(({ action, reactionHint }) => {
            const needle = query.trim().toLowerCase();
            if (!needle) return true;
            return action.toLowerCase().includes(needle) || reactionHint.toLowerCase().includes(needle);
        });
    }, [query]);

    const selectedAction = useMemo(() => actions.find(({ id }) => id === selectedId), [selectedId]);
    const selectedReaction = useMemo(() => reactions.find(({ id }) => id === selectedId), [selectedId]);

    const activityTrail = useMemo(() => {
        return actions.slice(0, 6).map(({ id, action }) => ({
            id,
            action,
            energy: (id * 17) % 100,
            band: threatBands[id % threatBands.length]
        }));
    }, []);

    return (
        <>
            <Head>
                <title>Next Guard Console</title>
                <link rel="preconnect" href="https://fonts.googleapis.com" />
                <link rel="preconnect" href="https://fonts.gstatic.com" crossOrigin="anonymous" />
                <link
                    href="https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;600&family=Instrument+Sans:wght@400;500&display=swap"
                    rel="stylesheet"
                />
            </Head>
            <main className="page">
                <section className="hero">
                    <div>
                        <p className="badge">Mission Control · {TOTAL_APIS} linked APIs</p>
                        <h1>
                            Precision control for the <span>Conscious Rocket AI</span>
                        </h1>
                        <p className="lede">
                            Curate an action, predict its paired reaction, and route the guardrails before telemetry hits the wire.
                        </p>
                    </div>
                    <div className="heroCard">
                        <p className="metricLabel">Active pairing</p>
                        <p className="metricAction">{selectedAction?.action ?? 'unknown'}</p>
                        <p className="metricReaction">{selectedReaction?.reaction ?? 'No pairing detected'}</p>
                        <p className="metricFoot">Endpoint · {selectedAction?.reactionEndpoint}</p>
                    </div>
                </section>

                <section className="grid">
                    <article className="panel actions">
                        <header>
                            <h2>Action palette</h2>
                            <span>{filtered.length} visible</span>
                        </header>
                        <div className="search">
                            <input
                                type="search"
                                placeholder="Filter ignition, replication, guard rails..."
                                value={query}
                                onChange={(e) => setQuery(e.target.value)}
                            />
                        </div>
                        <div className="actionList">
                            {filtered.map(({ id, action, reactionHint }) => (
                                <button
                                    key={id}
                                    className={`actionItem ${id === selectedId ? 'active' : ''}`}
                                    onClick={() => setSelectedId(id)}
                                >
                                    <div>
                                        <p className="actionName">{action}</p>
                                        <p className="actionHint">↳ {reactionHint}</p>
                                    </div>
                                    <span className="pill">#{id.toString().padStart(2, '0')}</span>
                                </button>
                            ))}
                        </div>
                    </article>

                    <article className="panel detail">
                        <header>
                            <h2>Reaction dossier</h2>
                            <span>Deterministic · encrypted</span>
                        </header>
                        {selectedAction && selectedReaction ? (
                            <div className="detailBody">
                                <div className="detailCard">
                                    <p className="label">Action</p>
                                    <h3>{selectedAction.action}</h3>
                                    <p className="label">Reaction</p>
                                    <h3 className="reaction">{selectedReaction.reaction}</h3>
                                </div>
                                <dl>
                                    <div>
                                        <dt>Reaction endpoint</dt>
                                        <dd>{selectedAction.reactionEndpoint}</dd>
                                    </div>
                                    <div>
                                        <dt>Source</dt>
                                        <dd>{selectedReaction.actionSource}</dd>
                                    </div>
                                    <div>
                                        <dt>Status</dt>
                                        <dd className="status">{selectedReaction.status}</dd>
                                    </div>
                                </dl>
                            </div>
                        ) : (
                            <p className="empty">Choose an action to inspect its guard band.</p>
                        )}
                    </article>
                </section>

                <section className="timeline panel">
                    <header>
                        <h2>Signal telemetry</h2>
                        <span>Live heuristics</span>
                    </header>
                    <div className="timelineTrack">
                        {activityTrail.map((item) => (
                            <div key={item.id} className="timelineItem">
                                <div className="timelineHeader">
                                    <p className="timelineAction">{item.action}</p>
                                    <span>{item.band}</span>
                                </div>
                                <div className="energyBar">
                                    <div
                                        className="energyFill"
                                        style={{ width: `${item.energy}%`, background: signalColor(item.energy) }}
                                    />
                                </div>
                                <p className="energyLabel">{item.energy}% signal energy</p>
                            </div>
                        ))}
                    </div>
                </section>
            </main>

            <style jsx global>{`
                :root {
                    --bg: #050816;
                    --panel: rgba(11, 16, 34, 0.75);
                    --accent: #6c5ce7;
                    --accent-light: #a29bfe;
                    --text: #ecf2ff;
                    --muted: rgba(236, 242, 255, 0.64);
                    font-family: 'Space Grotesk', 'Instrument Sans', system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
                }
                body {
                    margin: 0;
                    min-height: 100vh;
                    background: radial-gradient(circle at top right, #1b1f3a, #050816 60%);
                    color: var(--text);
                }
                * {
                    box-sizing: border-box;
                }
            `}</style>

            <style jsx>{`
                .page {
                    padding: 4rem clamp(1rem, 4vw, 3rem) 5rem;
                    display: flex;
                    flex-direction: column;
                    gap: 2.5rem;
                    max-width: 1200px;
                    margin: 0 auto;
                }
                .hero {
                    display: flex;
                    gap: 2rem;
                    align-items: stretch;
                    flex-wrap: wrap;
                }
                .badge {
                    letter-spacing: 0.1em;
                    text-transform: uppercase;
                    color: var(--accent-light);
                    font-size: 0.8rem;
                    margin: 0 0 1rem;
                }
                h1 {
                    font-size: clamp(2.5rem, 4vw, 3.5rem);
                    margin: 0;
                    line-height: 1.15;
                }
                h1 span {
                    color: var(--accent);
                }
                .lede {
                    color: var(--muted);
                    max-width: 32ch;
                    margin-top: 1rem;
                    font-size: 1.05rem;
                }
                .heroCard {
                    flex: 1;
                    min-width: 260px;
                    padding: 1.5rem;
                    border-radius: 1.5rem;
                    background: linear-gradient(135deg, rgba(108, 92, 231, 0.2), rgba(95, 211, 197, 0.12));
                    border: 1px solid rgba(255, 255, 255, 0.08);
                    display: flex;
                    flex-direction: column;
                    justify-content: center;
                    gap: 0.5rem;
                }
                .metricLabel {
                    text-transform: uppercase;
                    letter-spacing: 0.08em;
                    font-size: 0.75rem;
                    color: var(--muted);
                }
                .metricAction {
                    font-size: 1.3rem;
                    margin: 0;
                }
                .metricReaction {
                    font-size: 1.15rem;
                    margin: 0;
                    color: var(--accent-light);
                }
                .metricFoot {
                    margin: 0;
                    font-size: 0.85rem;
                    color: var(--muted);
                }
                .grid {
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
                    gap: 1.5rem;
                }
                .panel {
                    background: var(--panel);
                    border-radius: 1.5rem;
                    padding: 1.5rem;
                    border: 1px solid rgba(255, 255, 255, 0.05);
                    backdrop-filter: blur(14px);
                }
                .panel header {
                    display: flex;
                    justify-content: space-between;
                    align-items: center;
                    margin-bottom: 1rem;
                    color: var(--muted);
                }
                .panel h2 {
                    margin: 0;
                    font-size: 1.2rem;
                    color: var(--text);
                }
                .actions .search input {
                    width: 100%;
                    padding: 0.85rem 1rem;
                    border-radius: 999px;
                    border: 1px solid rgba(255, 255, 255, 0.08);
                    background: rgba(255, 255, 255, 0.04);
                    color: var(--text);
                }
                .actions .search input:focus {
                    outline: none;
                    border-color: var(--accent);
                    background: rgba(255, 255, 255, 0.08);
                }
                .actionList {
                    margin-top: 1rem;
                    display: flex;
                    flex-direction: column;
                    gap: 0.75rem;
                    max-height: 420px;
                    overflow-y: auto;
                    padding-right: 0.25rem;
                }
                .actionItem {
                    border: none;
                    display: flex;
                    justify-content: space-between;
                    align-items: center;
                    padding: 1rem;
                    border-radius: 1rem;
                    background: rgba(255, 255, 255, 0.03);
                    color: var(--text);
                    cursor: pointer;
                    transition: transform 150ms ease, background 150ms ease;
                }
                .actionItem:hover {
                    background: rgba(255, 255, 255, 0.08);
                    transform: translateY(-2px);
                }
                .actionItem.active {
                    background: linear-gradient(120deg, rgba(108, 92, 231, 0.35), rgba(19, 207, 207, 0.2));
                    border: 1px solid rgba(108, 92, 231, 0.4);
                }
                .actionName {
                    margin: 0;
                    font-weight: 600;
                }
                .actionHint {
                    margin: 0.2rem 0 0;
                    color: var(--muted);
                    font-size: 0.9rem;
                }
                .pill {
                    border-radius: 999px;
                    border: 1px solid rgba(255, 255, 255, 0.2);
                    padding: 0.25rem 0.75rem;
                    font-size: 0.8rem;
                }
                .detailBody {
                    display: flex;
                    flex-direction: column;
                    gap: 1rem;
                }
                .detailCard {
                    background: rgba(255, 255, 255, 0.03);
                    padding: 1.25rem;
                    border-radius: 1rem;
                    border: 1px solid rgba(255, 255, 255, 0.05);
                }
                .label {
                    text-transform: uppercase;
                    font-size: 0.75rem;
                    letter-spacing: 0.08em;
                    color: var(--muted);
                    margin: 0 0 0.35rem;
                }
                h3 {
                    margin: 0;
                    text-transform: lowercase;
                    font-size: 1.4rem;
                }
                .reaction {
                    color: var(--accent-light);
                }
                dl {
                    margin: 0;
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
                    gap: 1rem;
                }
                dt {
                    font-size: 0.75rem;
                    color: var(--muted);
                    letter-spacing: 0.05em;
                    text-transform: uppercase;
                }
                dd {
                    margin: 0.25rem 0 0;
                    font-weight: 500;
                }
                .status {
                    color: #3dd598;
                }
                .empty {
                    margin: 0;
                    color: var(--muted);
                }
                .timelineTrack {
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
                    gap: 1.25rem;
                }
                .timelineItem {
                    padding: 1rem;
                    background: rgba(255, 255, 255, 0.03);
                    border: 1px solid rgba(255, 255, 255, 0.05);
                    border-radius: 1rem;
                }
                .timelineHeader {
                    display: flex;
                    justify-content: space-between;
                    align-items: baseline;
                    margin-bottom: 0.5rem;
                }
                .timelineAction {
                    margin: 0;
                    font-weight: 600;
                }
                .energyBar {
                    height: 6px;
                    border-radius: 999px;
                    background: rgba(255, 255, 255, 0.1);
                    overflow: hidden;
                }
                .energyFill {
                    height: 100%;
                    border-radius: inherit;
                    transition: width 300ms ease;
                }
                .energyLabel {
                    margin: 0.65rem 0 0;
                    font-size: 0.85rem;
                    color: var(--muted);
                }
                @media (max-width: 640px) {
                    .page {
                        padding: 2.5rem 1rem 3rem;
                    }
                }
            `}</style>
        </>
    );
}
