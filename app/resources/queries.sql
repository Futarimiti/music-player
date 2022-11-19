-- get song and its artists
SELECT
	song.song_name AS "song name",
	song.artist_disp AS "artist disp name",
	artist.primary_name AS "artist prim name"
FROM
	song
	INNER JOIN song_artist ON song.id = song_artist.song_id
	INNER JOIN artist ON artist.id = song_artist.artist_id;

-- get aliases of artists
SELECT
	artist.primary_name AS "primary name",
	artist_alias.artist_alias AS "alias"
FROM
	artist
	INNER JOIN artist_alias ON artist.id = artist_alias.artist_id;
