/**
	Linkage learning test with ACO.
	AKM Khaled Ahsan Talukder
	08/12/2011, George Mason University
*/

#include <stdio.h>
#include <stdlib.h>
#include <randgen.h>
#include <probdef.h>
#include <aco.h>

#define MAXTRAILS 3000
/*INT_MAX      /* maximum number of ant trails */

double seed ;

Trail* newAlgo(KnapsackDescription *kpdsc, Trail** trail, long int MaxTrails, long int MaxEpoch, double tsize);
Trail* antSystem(KnapsackDescription *kpdsc, Trail** trail, long int MaxTrails, long int MaxEpoch);

int main(void)
{
	long int MaxEpoch, MaxTrails ;
	double tsize ;
	KnapsackDescription *kpdsc ;
	Trail **trail ;
	Trail *best ;

	seed = 0.8 ;

	initRandom() ;

	/* kpdsc = readKnapsackProblem("knapsacks//prob_04"); */
	/* kpdsc = readKnapsackProblem("knapsacks//100.1000.sc.in"); */
	kpdsc = readKnapsackProblem("knapsacks//100.in");

	dumpOnlyComponents(kpdsc); printf("\n");
	
	MaxTrails = 10 ;
	MaxEpoch = 10000 ;
	trail = (Trail**)malloc(sizeof(Trail*)*MaxTrails) ;
	tsize = 2.0 ;

	//best = newAlgo(kpdsc, trail, MaxTrails, MaxEpoch, tsize);
	best = antSystem(kpdsc, trail, MaxTrails, MaxEpoch);
	printf("Best: \n"); dumpTrail(best); 

	printf("Components: \n"); dumpOnlyComponents(kpdsc); printf("\n"); 

	free(trail);
	freeKnapsackDescription(kpdsc);
	freeTrail(best);
	return 0 ;
}

Trail* antSystem(KnapsackDescription *kpdsc, Trail** trail, long int MaxTrails, long int MaxEpoch)
{
	long int epoch, i ;
	Trail *best ;
	Trail *greedy ;
	double tau ;

	best = newTrail();

	greedy = constructGreedySolution(kpdsc);
	//tau = MaxTrails/greedy->totalValue ;
	//initializePheromone(kpdsc, tau);
	freeTrail(greedy);

	/*for( epoch = 0 ; epoch < MaxEpoch ; epoch++ )
	/*while(1)
	{
		printf("--------------------- Epoch %ld ------------------------ \n", epoch);
		for( i = 0 ; i < MaxTrails ; i++ )
		{
			if( epoch == 0 )
				trail[i] = initialRandomWalk(kpdsc);
			else
				/*trail[i] = constructSolutionUsingSizedTournament(kpdsc, tsize);*/
				//trail[i] = constructSolutionUsingAS(kpdsc);

			/*calculateFitness(trail[i], kpdsc) ;
			//updatePheromone(trail[i], kpdsc) ;
			if( trail[i]->fitness >= best->fitness)
			{
				freeTrail(best);
				best = clone(trail[i]) ;
			}
		}

		for( i = 0 ; i < MaxTrails ; i++ )
			freeTrail(trail[i]);

		printf("Best Trail at %ld:\n", epoch); dumpTrail((void*)best); printf("\n");
	}*/
	
	return best ;
}

Trail* newAlgo(KnapsackDescription *kpdsc, Trail** trail, long int MaxTrails, long int MaxEpoch, double tsize)
{
	long int epoch, i, j;
	Trail *best ;
	double u_i, p_i, d_i ;

	best = newTrail();

	for( epoch = 0 ; epoch < MaxEpoch ; epoch++ )
	/*while(1)*/
	{
		printf("--------------------- Epoch %ld ------------------------ \n", epoch);
		for( i = 0 ; i < MaxTrails ; i++ )
		{
			if( epoch == 0 )
				trail[i] = initialRandomWalk(kpdsc);
			else
				/*trail[i] = constructSolutionUsingSizedTournament(kpdsc, tsize);*/
				trail[i] = constructSolutionUsingSimpleTournament(kpdsc);

			calculateFitness(trail[i], kpdsc) ;
			if( trail[i]->fitness >= best->fitness)
			{
				freeTrail(best);
				best = clone(trail[i]) ;
			}
		}

		for( i = 0 ; i < kpdsc->length ; i++)
		{
			kpdsc->c[i].d = NEG_INFINITY ;
			for( j = 0 ; j < MaxTrails ; j++ )
			{
				if( isUsed(&kpdsc->c[i], trail[j]) )
				{
					d_i = kpdsc->c[i].d ;
					kpdsc->c[i].d = max(d_i, trail[j]->fitness);		
				}
			}
			if( kpdsc->c[i].d > NEG_INFINITY )
			{
				kpdsc->c[i].u += 1 ;
				u_i  = kpdsc->c[i].u ;
				p_i = kpdsc->c[i].p ;
				d_i = kpdsc->c[i].d ;
				kpdsc->c[i].p = (1 - max(1.0/u_i, epsilon))*p_i + max(1.0/u_i, epsilon)*d_i ;
			}
		}
		
		for( i = 0 ; i < MaxTrails ; i++ )
			freeTrail(trail[i]);

		printf("Best Trail at %ld:\n", epoch); dumpTrail((void*)best); printf("\n");
		if( tsize < kpdsc->length )
			tsize += delta ;
	}

	return best ;
}	
