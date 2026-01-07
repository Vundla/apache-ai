const fs = require('fs');
const path = require('path');
const { Client } = require('cassandra-driver');

async function main() {
    const ca = fs.readFileSync(path.join(__dirname, '../../../config/tls/ca.crt'));
    const client = new Client({
        contactPoints: ['127.0.0.1'],
        localDataCenter: 'datacenter1',
        sslOptions: {
            ca: [ca],
            rejectUnauthorized: true
        },
        protocolOptions: { port: 9042 },
        credentials: { username: 'observer_writer', password: 'observer_pass' }
    });

    await client.connect();
    const rs = await client.execute('SELECT cluster_name, data_center FROM system.local');
    console.log('Connected to cluster:', rs.rows[0]);
    await client.shutdown();
}

main().catch((e) => {
    console.error('Cassandra TLS test failed:', e);
    process.exit(1);
});
