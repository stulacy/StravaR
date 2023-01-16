CREATE TABLE activity_types (
  activity_type TEXT PRIMARY KEY
);

INSERT INTO activity_types VALUES('Run');
INSERT INTO activity_types VALUES('Ride');

CREATE TABLE config (
    property TEXT,
    value TEXT
);

CREATE TABLE activities (
  activity_id INTEGER PRIMARY KEY,
  activity_type TEXT,
  name TEXT,
  start_time TIMESTAMP,
  distance REAL,
  duration REAL,
  elevation REAL,
  FOREIGN KEY(activity_type) REFERENCES activity_types(activity_type)
);

CREATE TABLE heartrate (
  activity_id,
  time TIMESTAMP,
  heartrate REAL NOT NULL,
  PRIMARY KEY(activity_id, time),
  FOREIGN KEY(activity_id) REFERENCES activities(activity_id)
);

CREATE TABLE location (
  activity_id INTEGER,
  time TIMESTAMP,
  lat REAL NOT NULL,
  lon REAL NOT NULL,
  PRIMARY KEY(activity_id, time),
  FOREIGN KEY(activity_id) REFERENCES activities(activity_id)
);

CREATE TABLE athlete (
  athelete_id INTEGER PRIMARY KEY,
  height REAL,
  weight REAL,
  maxHR REAL,
  restHR REAL,
  thresholdHR REAL
);
