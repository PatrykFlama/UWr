MATCH (n)
DETACH DELETE n;

// Create test data
CREATE (a1:Person {name: "Actor1"}),
       (a2:Person {name: "Actor2"}),
       (a3:Person {name: "Actor3"}),
       (a4:Person {name: "Actor4"}),
       (d1:Person {name: "Director1"}),
       (d2:Person {name: "Director2"}),
       (m1:Movie {title: "Movie1"}),
       (m2:Movie {title: "Movie2"}),
       (m3:Movie {title: "Movie3"}),
       (a1)-[:ACTED_IN]->(m1),
       (a2)-[:ACTED_IN]->(m1),
       (a2)-[:ACTED_IN]->(m2),
       (a2)-[:ACTED_IN]->(m3),
       (a2)-[:DIRECTED]->(m1),
       (a3)-[:ACTED_IN]->(m3),
       (d1)-[:DIRECTED]->(m1),
       (d2)-[:DIRECTED]->(m2),
       (d2)-[:DIRECTED]->(m3);


// 1. Return the movies where person X acted in:
MATCH (p:Person {name: "Actor2"})-[:ACTED_IN]->(m:Movie)
RETURN m;

// 2. Return the movies where person X was both the actor and the director:
MATCH (p:Person {name: "Actor2"})-[:ACTED_IN]->(m:Movie)<-[:DIRECTED]-(p)
RETURN m;

// 3. Return actors who didn’t play in any movie:
MATCH (p:Person)
WHERE NOT (p)-[:ACTED_IN]->(:Movie)
RETURN p;

// 4. Return actors who played in more than 2 movies:
MATCH (p:Person)-[:ACTED_IN]->(m:Movie)
WITH p, count(m) as movieCount
WHERE movieCount > 2
RETURN p;

// 5. Return movies with the larger number of actors:
MATCH (m:Movie)<-[:ACTED_IN]-(p:Person)
WITH m, count(p) as actorCount
ORDER BY actorCount DESC
RETURN m;