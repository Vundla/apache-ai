-- Ownership: Apache, Capaciti, ALX
-- CockroachDB primary schema for encrypted telemetry and AI insights (dev, insecure). Enable TLS in prod.

CREATE DATABASE IF NOT EXISTS rocket_ai;
SET DATABASE = rocket_ai;

CREATE TABLE IF NOT EXISTS engine_telemetry (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ts TIMESTAMPTZ NOT NULL DEFAULT now(),
  component_id STRING NOT NULL,
  metrics JSONB NOT NULL,
  encrypted_payload BYTES NOT NULL,
  signature BYTES NOT NULL,
  INDEX idx_ts (ts DESC),
  INDEX idx_component_ts (component_id, ts DESC)
);

CREATE TABLE IF NOT EXISTS ai_insights (
  insight_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ts TIMESTAMPTZ NOT NULL DEFAULT now(),
  kind STRING NOT NULL,
  confidence FLOAT8 NOT NULL,
  encrypted_insight BYTES NOT NULL
);
