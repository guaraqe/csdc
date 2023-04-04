CREATE TYPE election_type AS enum (
  'Jugement Majoroitaire',
  'Consensus majoritaire'
  );


CREATE TABLE elections
(
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(), 
    unit uuid NOT NULL REFERENCES units(id) ON DELETE CASCADE,
    title text NOT NULL,
    description text NOT NULL,
    choices text[] NOT NULL,
    election_type election_type NOT NULL,
    visible_votes boolean NOT NULL,
    ending_at timestamptz NOT NULL,
    result text,
    result_computed_at timestamptz
);


CREATE TABLE votes
(
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(), 
    election uuid NOT NULL REFERENCES elections(id) ON DELETE CASCADE,
    vote jsonb NOT NULL
);

CREATE TABLE voters
(
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(), 
    election uuid NOT NULL REFERENCES elections(id) ON DELETE CASCADE,
    person uuid NOT NULL REFERENCES persons(id) ON DELETE CASCADE,
    voted_at timestamptz NOT NULL,
    vote uuid REFERENCES votes(id)
);


