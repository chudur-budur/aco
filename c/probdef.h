/**
	Linkage learning test with ACO.
	This file contains the problem
	definition for 0-1 Knapsack Problem.
	AKM Khaled Ahsan Talukder
	08/12/2011, George Mason University
*/

#ifndef _PROBDEF_H_
#define _PROBDEF_H_

#define ALLOWED 1
#define NOT_ALLOWED 0

/** 
	Encapsulates a component 
**/
struct Component
{
	int ID ;     /* component ID */
	double value ;  /* profit value for this component */
	double weight ; /* weight of this component */
	double p ; /* pheromone value for this component */
	double d ; /* delta of pheromone for this component */
	int u ;   /* usage count for this component */
	double eta ; /* heuristic value: value/weight */
};
typedef struct Component Component ;

/** 
	Transition probability matrix that represents
	which component should likely to come after one 
	particular component
**/
struct PheromoneGraph
{
	/* 
		prob(i|j) or prob(j|i): probability of 
		component i comes after component j or vice-versa
	*/
	double **e ; 
	int row ; /* row index for the component with the max pheromone value */	
	int col ; /* col index for the component with the max pheromone value */
	double maxph ; /* the max pheromone value */
};
typedef struct PheromoneGraph PheromoneGraph ;

/**
	Adjacency graph for 0-1 Knapsack Problem	
**/
struct AdjacencyGraph
{
	int **e ; /* e(i,j): If component i can come after component j */
};
typedef struct AdjacencyGraph AdjacencyGraph ;

/**      
	Encapsulates a 0-1 Knapsack Problem Description with --
	Array of components
	Knapsack Capacity
	Graph to store the adjacency (not required)
**/
struct KnapsackDescription
{
	int length ;
	Component *c ;
	double capacity ;
	PheromoneGraph ph ;
	AdjacencyGraph adj ;
};
typedef struct KnapsackDescription KnapsackDescription ;

KnapsackDescription* readKnapsackProblem(char *fileName);
void freeKnapsackDescription(KnapsackDescription *kpdsc);
void dumpKnapsackFullDescription(KnapsackDescription *kpdsc);
void dumpOnlyComponents(KnapsackDescription *kpdsc);

#endif
