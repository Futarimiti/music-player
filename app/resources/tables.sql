-- vim! sql_flavor: sqlite
-- relation betw artist and alias(es)
-- an artist could have multiple aliases
DROP TABLE IF EXISTS artist_alias;

CREATE TABLE
	artist_alias (
		id INTEGER PRIMARY key,
		artist_id INTEGER NOT NULL,
		artist_alias TEXT NOT NULL,
		FOREIGN KEY (artist_id) REFERENCES artist (id)
	);

-- relation betw song and artist(s)
-- a song could have multiple artists, and an artist could have multiple songs
DROP TABLE IF EXISTS song_artist;

CREATE TABLE
	song_artist (
		id INTEGER PRIMARY key,
		song_id INTEGER NOT NULL,
		artist_id INTEGER NOT NULL,
		FOREIGN KEY (song_id) REFERENCES song (id),
		FOREIGN KEY (artist_id) REFERENCES artist (id)
	);

-- relation betw song and genre(s)
-- a song may have multiple genres, and a genre includes multiple songs
DROP TABLE IF EXISTS song_genre;

CREATE TABLE
	song_genre (
		id INTEGER PRIMARY KEY,
		song_id INTEGER NOT NULL,
		genre_id INTEGER NOT NULL,
		FOREIGN KEY (genre_id) REFERENCES genre (id),
		FOREIGN KEY (song_id) REFERENCES song (id)
	);

-- relation betw song and album
-- an album may include multiple songs
DROP TABLE IF EXISTS song_album;

CREATE TABLE
	song_album (
		id INTEGER PRIMARY KEY,
		song_id INTEGER NOT NULL,
		album_id INTEGER NOT NULL,
		FOREIGN KEY (song_id) REFERENCES song (id),
		FOREIGN KEY (album_id) REFERENCES album (id)
	);

DROP TABLE IF EXISTS album;

CREATE TABLE
	album (id INTEGER PRIMARY KEY, album_name text NOT NULL);

DROP TABLE IF EXISTS genre;

CREATE TABLE
	genre (id INTEGER PRIMARY KEY, genre_name TEXT NOT NULL);

DROP TABLE IF EXISTS artist;

CREATE TABLE
	artist (
		id INTEGER PRIMARY KEY,
		primary_name TEXT NOT NULL
	);

DROP TABLE IF EXISTS song;

CREATE TABLE
	song (
		id INTEGER PRIMARY KEY,
		song_name TEXT NOT NULL,
		artist_disp TEXT NOT NULL,
		source_url TEXT,
		filename TEXT -- including extension!
	);

-- values
--  INSERT INTO
--  artist (primary_name)
--  VALUES
--  ('Bernis'),
--  ('ColBreakz'),
--  ('Protolizard'),
--  ('DJ Counterforce'),
--  ('ARForest'),
--  ('かめりあ');
/*
INSERT INTO
artist_alias (artist_id, artist_alias)
VALUES
(
(
SELECT
id
FROM
artist
WHERE
primary_name = 'Bernis'
),
'Yuri Yuzuriha'
),
(
(
SELECT
id
FROM
artist
WHERE
primary_name = 'Bernis'
),
'Yuri Yuzriha'
),
(
(
SELECT
id
FROM
artist
WHERE
primary_name = 'かめりあ'
),
'Camellia'
);

INSERT INTO
song (song_name, artist_disp)
VALUES
('Terminal Shore', 'Bernis'),
('Hemisphere', 'ARForest'),
('Deception', 'Protolizard & ColBreakz');

INSERT INTO
song_artist (song_id, artist_id)
VALUES
(
(
SELECT
id
FROM
song
WHERE
song_name = 'Terminal Shore'
),
(
SELECT
id
FROM
artist
WHERE
primary_name = 'Bernis'
)
),
(
(
SELECT
id
FROM
song
WHERE
song_name = 'Hemisphere'
),
(
SELECT
id
FROM
artist
WHERE
primary_name = 'ARForest'
)
),
(
(
SELECT
id
FROM
song
WHERE
song_name = 'Deception'
),
(
SELECT
id
FROM
artist
WHERE
primary_name = 'Protolizard'
)
),
(
(
SELECT
id
FROM
song
WHERE
song_name = 'Deception'
),
(
SELECT
id
FROM
artist
WHERE
primary_name = 'ColBreakz'
)
);
 */
