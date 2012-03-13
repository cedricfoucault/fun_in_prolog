/**
	The graph representing the map of Paris's underground.
	The edges tell if two lines are connected,
	with the corresponding station(s) as an optional third argument.
*/

vertices_metro([l1, l2, l3, l3bis, l4, l5, l6, l7, l7bis, l8, l9, l10, l11, l12, l13, l14]).

edge_metro(l1,l2).
edge_metro(l1,l4).
edge_metro(l1,l5).
edge_metro(l1,l6).
edge_metro(l1,l7).
edge_metro(l1,l8).
edge_metro(l1,l9).
edge_metro(l1,l11).
edge_metro(l1,l12).
edge_metro(l1,l13).
edge_metro(l1,l14).
edge_metro(l2,l3).
edge_metro(l2,l4).
edge_metro(l2,l5).
edge_metro(l2,l6).
edge_metro(l2,l7).
edge_metro(l2,l7bis).
edge_metro(l2,l9).
edge_metro(l2,l11).
edge_metro(l2,l12).
edge_metro(l2,l13).
edge_metro(l3,l3bis).
edge_metro(l3,l4).
edge_metro(l3,l5).
edge_metro(l3,l6).
edge_metro(l3,l7).
edge_metro(l3,l8).
edge_metro(l3,l9).
edge_metro(l3,l11).
edge_metro(l3,l12).
edge_metro(l3,l13).
edge_metro(l3,l14).
edge_metro(l3bis,l11).
edge_metro(l4,l5).
edge_metro(l4,l6).
edge_metro(l4,l7).
edge_metro(l4,l8).
edge_metro(l4,l9).
edge_metro(l4,l10).
edge_metro(l4,l11).
edge_metro(l4,l12).
edge_metro(l4,l13).
edge_metro(l4,l14).
edge_metro(l5,l6).
edge_metro(l5,l7).
edge_metro(l5,l7bis).
edge_metro(l5,l8).
edge_metro(l5,l9).
edge_metro(l5,l10).
edge_metro(l5,l11).
edge_metro(l6,l7).
edge_metro(l6,l8).
edge_metro(l6,l9).
edge_metro(l6,l10).
edge_metro(l6,l12).
edge_metro(l6,l13).
edge_metro(l6,l14).
edge_metro(l7,l7bis).
edge_metro(l7,l8).
edge_metro(l7,l9).
edge_metro(l7,l10).
edge_metro(l7,l11).
edge_metro(l7,l14).
edge_metro(l7bis,l11).
edge_metro(l8,l9).
edge_metro(l8,l10).
edge_metro(l8,l11).
edge_metro(l8,l12).
edge_metro(l8,l13).
edge_metro(l8,l14).
edge_metro(l9,l10).
edge_metro(l9,l11).
edge_metro(l9,l13).
edge_metro(l9,l14).
edge_metro(l10,l12).
edge_metro(l10,l13).
edge_metro(l11,l1).
edge_metro(l11,l14).
edge_metro(l12,l13).
edge_metro(l12,l14).
edge_metro(l13,l14).

edge_metro(l1,l2,'CDG Etoile').
edge_metro(l1,l2,'Nation').
edge_metro(l1,l4,'Chatelet').
edge_metro(l1,l5,'Bastille').
edge_metro(l1,l6,'Nation').
edge_metro(l1,l6,'CDG Etoile').
edge_metro(l1,l7,'Palais Royal').
edge_metro(l1,l7,'Chatelet').
edge_metro(l1,l8,'Reuilly Diderot').
edge_metro(l1,l8,'Concorde').
edge_metro(l1,l8,'Bastille').
edge_metro(l1,l9,'Nation').
edge_metro(l1,l9,'Roosevelt').
edge_metro(l1,l11,'Hotel de Ville').
edge_metro(l1,l11,'Chatelet').
edge_metro(l1,l12,'Concorde').
edge_metro(l1,l13,'Champs Elysée').
edge_metro(l1,l14,'Chatelet').
edge_metro(l1,l14,'Gare de Lyon').
edge_metro(l2,l3,'Villiers').
edge_metro(l2,l3,'Pere Lachaise').
edge_metro(l2,l4,'Barbes Rochechouard').
edge_metro(l2,l5,'Jaures').
edge_metro(l2,l5,'Stalingrad').
edge_metro(l2,l6,'Nation').
edge_metro(l2,l6,'CDG Etoile').
edge_metro(l2,l7,'Stalingrad').
edge_metro(l2,l7bis,'Jaures').
edge_metro(l2,l9,'Nation').
edge_metro(l2,l11,'Belleville').
edge_metro(l2,l12,'Pigalle').
edge_metro(l2,l13,'Clichy').
edge_metro(l3,l3bis,'Gambetta').
edge_metro(l3,l4,'Reaumur Sebastopol').
edge_metro(l3,l5,'Republique').
edge_metro(l3,l6,'wtf').
edge_metro(l3,l7,'Opera').
edge_metro(l3,l8,'Opera').
edge_metro(l3,l8,'Republique').
edge_metro(l3,l9,'Havre Caumartin').
edge_metro(l3,l9,'Republique').
edge_metro(l3,l11,'Republique').
edge_metro(l3,l11,'Arts et metiers').
edge_metro(l3,l12,'Saint Lazare').
edge_metro(l3,l13,'Saint Lazare').
edge_metro(l3,l14,'Saint Lazare').
edge_metro(l3bis,l11,'Porte des Lilas').
edge_metro(l4,l5,'Gare du Nord').
edge_metro(l4,l5,'Gare Est').
edge_metro(l4,l6,'Denfert Rocherau').
edge_metro(l4,l6,'Raspail').
edge_metro(l4,l6,'Gare Montparnasse').
edge_metro(l4,l7,'Gare Est').
edge_metro(l4,l7,'Chatelet').
edge_metro(l4,l8,'Strasbourg').
edge_metro(l4,l9,'Strasbourg').
edge_metro(l4,l10,'Odeon').
edge_metro(l4,l11,'Chatelet').
edge_metro(l4,l12,'Marcadet').
edge_metro(l4,l12,'Gare Montparnasse').
edge_metro(l4,l13,'Gare Montparnasse').
edge_metro(l4,l14,'Chatelet').
edge_metro(l5,l6,'Place Italie').
edge_metro(l5,l7,'Place Italie').
edge_metro(l5,l7,'Gare Est').
edge_metro(l5,l7,'Stalingrad').
edge_metro(l5,l7bis,'Jaures').
edge_metro(l5,l8,'Republique').
edge_metro(l5,l8,'Bastille').
edge_metro(l5,l9,'Republique').
edge_metro(l5,l9,'Oberkampf').
edge_metro(l5,l10,'Gare Austerlitz').
edge_metro(l5,l11,'Republique').
edge_metro(l6,l7,'Place Italie').
edge_metro(l6,l8,'La Motte Picquet Grenelle').
edge_metro(l6,l9,'Nation').
edge_metro(l6,l9,'Trocadero').
edge_metro(l6,l10,'La Motte Picquet Grenelle').
edge_metro(l6,l12,'Pasteur').
edge_metro(l6,l12,'Gare Montparnasse').
edge_metro(l6,l13,'Gare Montparnasse').
edge_metro(l6,l14,'Bercy').
edge_metro(l7,l7bis,'Louis Blanc').
edge_metro(l7,l8,'Opera').
edge_metro(l7,l9,'La Fayette').
edge_metro(l7,l10,'Jussieu').
edge_metro(l7,l11,'Chatelet').
edge_metro(l7,l14,'Chatelet').
edge_metro(l7bis,l11,'Place des Fetes').
edge_metro(l8,l9,'Richelieu Drouot').
edge_metro(l8,l9,'Strasbourg').
edge_metro(l8,l9,'Republique').
edge_metro(l8,l10,'La Motte Picquet Grenelle').
edge_metro(l8,l11,'Republique').
edge_metro(l8,l12,'Concorde').
edge_metro(l8,l12,'Madeleine').
edge_metro(l8,l13,'invalide').
edge_metro(l8,l14,'Madeleine').
edge_metro(l9,l10,'Michel Ange').
edge_metro(l9,l11,'Republique').
edge_metro(l9,l13,'Miromesnil').
edge_metro(l9,l14,'Saint Lazare').
edge_metro(l10,l12,'Babylone').
edge_metro(l10,l13,'Duroc').
edge_metro(l11,l14,'Chatelet').
edge_metro(l12,l13,'Gare Montparnasse').
edge_metro(l12,l14,'Madeleine').
edge_metro(l12,l14,'Saint Lazare').
edge_metro(l13,l14,'Saint Lazare').

