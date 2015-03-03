/**
	Linkage learning test with ACO.
	AKM Khaled Ahsan Talukder
	08/12/2011, George Mason University
*/

#ifndef _ACO_H_
#define _ACO_H_

#include <limits.h>
#include <math.h>
#include <probdef.h>
#include <list.h>

/** Problem specific parameters **/
#define LATENCY 0          /* extra size of the knapsack
		              beyond the original capacity
		              during the initial random walk */

/** Traditional AS parameters **/
#define EPSILON 0.0001     /* Initial pheromone value*/
#define rho 0.5            /* pheromone reduction value */
#define alpha 0.90         /* priority constant for pheromone value */
#define beta 0.95          /* priority constant for 
			      the heuristic function value */

/** Parameters relevant to the new algorithm **/
#define NEG_INFINITY (-1.0 * HUGE_VAL) 
#define epsilon 0.01 /* learning rate */
#define m 1	     /* min number of test */
#define delta 0.01    /* tournament size increment */

struct KnapsackDescription ;

/**
	Encapsulates a particular trail
**/
struct Trail
{
	List *lst ;
	double totalValue ;
	double totalWeight ;
	double fitness ;
};
typedef struct Trail Trail ;

Trail* newTrail();
void freeTrail(Trail *trail);
Trail* clone(Trail *trail);
void dumpComponent(void *data);
void addComponent(Trail *trail, Component *c);
void removeComponent(Trail *trail);
int isSame(void *data1, void *data2);
void dumpTrail(void *data);

Trail* initialRandomWalk(struct KnapsackDescription *kpdsc);
List* createNeighbourList(Component *c, List *MasterList, struct KnapsackDescription *kpdsc);
void calculateFitness(Trail *trail, struct KnapsackDescription *kpdsc);
Trail* constructSolutionUsingSizedTournament(struct KnapsackDescription *kpdsc, double tsize);
Trail* constructSolutionUsingSimpleTournament(struct KnapsackDescription *kpdsc);
Component* tournamentSelect(List *list, double size);
Component* tournament(Component c1, Component c2);

Trail* constructGreedySolution(struct KnapsackDescription *kpdsc);
int compareEta(void *d1, void *d2);

int isUsed(Component *c, Trail *trail);
double max(double a, double b);

#endif
