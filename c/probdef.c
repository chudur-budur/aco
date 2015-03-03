/**
	Linkage learning test with ACO.
	This file contains the problem
	definition for 0-1 Knapsack Problem.
	AKM Khaled Ahsan Talukder
	08/12/2011, George Mason University
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <probdef.h>

/**
	Reads a 0-1 Knapsack Problem description from the file
	and allocates all necessary memories.
	File format:
	<number of components>
	<component ID> <component value> <component weight>
	...
	...
	...
	<knapsack capacity>
**/
KnapsackDescription* readKnapsackProblem(char *fileName)
{
	int i, j;
	FILE *fp ;
	KnapsackDescription *kpdsc ;
	kpdsc = (KnapsackDescription*)malloc(sizeof(KnapsackDescription));
	fp = fopen((const char*)fileName, "r");
	if( fp == NULL )
	{	
		printf("Error: Couldn't open file: %s\n", fileName);
		exit(0);
	}
	else
	{
		fscanf(fp, "%d", &(kpdsc->length));
		kpdsc->c = (Component*)malloc(sizeof(Component)*kpdsc->length);
		for(i = 0 ; i < kpdsc->length ; i++)
		{
			fscanf(fp, "%d %lf %lf", &(kpdsc->c[i].ID), 
				&(kpdsc->c[i].value), &(kpdsc->c[i].weight));
		}
		fscanf(fp, "%lf", &(kpdsc->capacity));
		for( i = 0 ; i < kpdsc->length ; i++ )
		{
			kpdsc->c[i].p = 0.0 ;
			kpdsc->c[i].d = 0.0 ;
			kpdsc->c[i].u = 0 ;
			kpdsc->c[i].eta = kpdsc->c[i].value/kpdsc->c[i].weight ;
		}
	}
	kpdsc->ph.e = malloc(sizeof(double*)*kpdsc->length) ;
	kpdsc->adj.e = malloc(sizeof(int*)*kpdsc->length) ;
	for(i = 0 ; i < kpdsc->length ; i++)
	{
		kpdsc->ph.e[i] = malloc(sizeof(double)*kpdsc->length) ;
		kpdsc->adj.e[i] = malloc(sizeof(int)*kpdsc->length) ;
	}
	for(i = 0 ; i < kpdsc->length ; i++)
	{
		for(j = 0 ; j < kpdsc->length ; j++)
		{
			if( i != j )
			{
				kpdsc->ph.e[i][j] = 0.0 ;
				kpdsc->adj.e[i][j] = ALLOWED ;
			}
			else
			{
				kpdsc->ph.e[i][j] = 0.0 ;
				kpdsc->adj.e[i][j] = NOT_ALLOWED ;
			}
		}
	}
	kpdsc->ph.row = 0 ;
	kpdsc->ph.col = 0 ;
	kpdsc->ph.maxph = 0.0 ;
	return kpdsc ;
}

void freeKnapsackDescription(KnapsackDescription *kpdsc)
{
	int i ;
	for( i = 0 ; i < kpdsc->length ; i++)
	{
		free(kpdsc->adj.e[i]);
		free(kpdsc->ph.e[i]);
	}
	free(kpdsc->adj.e);
	free(kpdsc->ph.e);
	free(kpdsc->c);
	free(kpdsc);
}

/**
	Prints the 0-1 Knapsack Problem description
	along with the adjacency and the transition
	matrices.
**/
void dumpKnapsackDescription(KnapsackDescription *kpdsc)
{
	int i, j;
	printf("Total item: %d\n", kpdsc->length);

	for(i = 0 ; i < kpdsc->length ; i++)
		printf("Item: %d, Value: %.2f, Weight: %.2f, p: %.2f, d: %.2f, u: %d, eta: %.2f\n", 
		kpdsc->c[i].ID, kpdsc->c[i].value, kpdsc->c[i].weight, 
		kpdsc->c[i].p, kpdsc->c[i].d, kpdsc->c[i].u, kpdsc->c[i].eta);
	printf("Capacity: %.2f\n", kpdsc->capacity);

	printf("Adjacency(Neighbourhood) Matrix: \n");
	for(i = 0 ; i < kpdsc->length ; i++)
	{
		for(j = 0 ; j < kpdsc->length ; j++)
			printf("%d ", kpdsc->adj.e[i][j]);
		printf("\n");
	}

	printf("2nd Order Pheromone Matrix: \n");
	for(i = 0 ; i < kpdsc->length ; i++)
	{
		for(j = 0 ; j < kpdsc->length ; j++)
			printf("%.2f ", kpdsc->ph.e[i][j]);
		printf("\n");
	}
	printf("--> max [%d][%d]: %.2f\n", kpdsc->ph.row, kpdsc->ph.col, kpdsc->ph.maxph);
}


/**
	It only shows the components and their information
**/
void dumpOnlyComponents(KnapsackDescription *kpdsc)
{
	int i ;
	printf("Total item: %d\n", kpdsc->length);

	for(i = 0 ; i < kpdsc->length ; i++)
		printf("Item: %d, Value: %.2f, Weight: %.2f, p: %.2f, d: %.2f, u: %d, eta: %.2f\n", 
		kpdsc->c[i].ID, kpdsc->c[i].value, kpdsc->c[i].weight, 
		kpdsc->c[i].p, kpdsc->c[i].d, kpdsc->c[i].u, kpdsc->c[i].eta);
	printf("Capacity: %.2f\n", kpdsc->capacity);
}
