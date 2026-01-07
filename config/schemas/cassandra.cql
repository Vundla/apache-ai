// Ownership: Apache, Capaciti, ALX
// Cassandra wide-column replica for high-throughput time-series reads

CREATE KEYSPACE IF NOT EXISTS rocket_ai WITH replication = {
  'class': 'SimpleStrategy', 'replication_factor': 1
};

CREATE TABLE IF NOT EXISTS rocket_ai.sensor_readings (
  component_id text,
  sensor_type text,
  reading_time timestamp,
  value double,
  unit text,
  encrypted_context blob,
  PRIMARY KEY ((component_id, sensor_type), reading_time)
) WITH CLUSTERING ORDER BY (reading_time DESC);

CREATE TABLE IF NOT EXISTS rocket_ai.ai_patterns (
  pattern_id uuid,
  discovered_at timestamp,
  pattern_type text,
  confidence double,
  encrypted_features blob,
  PRIMARY KEY (pattern_id, discovered_at)
) WITH CLUSTERING ORDER BY (discovered_at DESC);
