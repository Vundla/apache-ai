-- Ownership: Apache, Capaciti, ALX
-- Postgres replica for analytics and model metadata

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS telemetry_hypertable (
  time TIMESTAMPTZ NOT NULL,
  component_id TEXT NOT NULL,
  metrics JSONB NOT NULL,
  encrypted_raw BYTEA
);

CREATE TABLE IF NOT EXISTS ml_models (
  model_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  model_name TEXT NOT NULL,
  version INTEGER NOT NULL,
  architecture TEXT NOT NULL,
  trained_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  encrypted_weights BYTEA NOT NULL,
  performance_metrics JSONB,
  UNIQUE(model_name, version)
);

CREATE TABLE IF NOT EXISTS observer_state (
  state_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ts TIMESTAMPTZ NOT NULL DEFAULT now(),
  attention_focus JSONB NOT NULL,
  prediction_errors JSONB,
  encrypted_internal_state BYTEA NOT NULL
);
