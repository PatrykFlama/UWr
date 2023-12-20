// clear databse
MATCH (n)
DETACH DELETE n;

// ------------------------------
// add 2 new actors
CREATE (:Actor {name: "Actor1"});
CREATE (:Actor {name: "Actor2"});

// add 2 new movies
CREATE (:Movie {title: "Movie1"});
CREATE (:Movie {title: "Movie2"});

// add 2 new properties to 1 movie
MATCH (m:Movie {title: "Movie1"})
SET m.genre = "Action", m.releaseYear = 2022;

// add 2 new acted in relations to the existing nodes
MATCH (a:Actor {name: "Actor1"}), (m:Movie {title: "Movie1"})
CREATE (a)-[:ACTED_IN]->(m);

MATCH (a:Actor {name: "Actor2"}), (m:Movie {title: "Movie2"})
CREATE (a)-[:ACTED_IN]->(m);

// update 1 movie property
MATCH (m:Movie {title: "Movie1"})
SET m.genre = "Thriller";

// remove 1 acted in relation
MATCH (a:Actor {name: "Actor1"})-[r:ACTED_IN]->(m:Movie {title: "Movie1"})
DELETE r;
