CREATE TABLE activity_types (
  activity_type TEXT PRIMARY KEY
);

CREATE TABLE config (
    property TEXT,
    value TEXT
);

CREATE TABLE activities (
  activity_id UBIGINT PRIMARY KEY,
  activity_type TEXT,
  name TEXT,
  start_time TIMESTAMP,
  distance DOUBLE,
  duration DOUBLE,
  elevation DOUBLE,
  FOREIGN KEY(activity_type) REFERENCES activity_types(activity_type)
);

CREATE TABLE heartrate (
  activity_id UBIGINT,
  time TIMESTAMP,
  heartrate DOUBLE NOT NULL,
  PRIMARY KEY(activity_id, time),
  FOREIGN KEY(activity_id) REFERENCES activities(activity_id)
);

CREATE TABLE fitness (
  activity_id UBIGINT PRIMARY KEY,
  hrss DOUBLE,
  FOREIGN KEY(activity_id) REFERENCES activities(activity_id)
);

CREATE TABLE location (
  activity_id UBIGINT,
  time TIMESTAMP,
  lat DOUBLE NOT NULL,
  lon DOUBLE NOT NULL,
  PRIMARY KEY(activity_id, time),
  FOREIGN KEY(activity_id) REFERENCES activities(activity_id)
);

CREATE TABLE athlete (
  athlete_id INTEGER PRIMARY KEY,
  height DOUBLE,
  weight DOUBLE,
  maxHR DOUBLE,
  restHR DOUBLE,
  thresholdHR DOUBLE
);

INSERT INTO activity_types(activity_type) VALUES('Run');
INSERT INTO activity_types(activity_type) VALUES('Ride');
