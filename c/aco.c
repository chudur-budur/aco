/**
	Linkage learning test with ACO.
	AKM Khaled Ahsan Talukder
	08/12/2011, George Mason University
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <randgen.h>
#include <sort.h>
#include <list.h>
#include <probdef.h>
#include <aco.h>

/* creates a new trail */
Trail* newTrail()
{
	Trail *trail = (Trail*)malloc(sizeof(Trail));
	trail->lst = newList();
	trail->totalValue = 0.0 ;
	trail->totalWeight = 0.0 ;
	trail->fitness = 0.0 ;
	return trail ;
}

/* frees a trail */
void freeTrail(Trail *trail)
{
	freeList(trail->lst);
	free(trail);
}

/* clones a trail */
Trail* clone(Trail *trail)
{
	Trail *temp ;
	temp = newTrail();
	freeList(temp->lst);
	temp->lst = cloneList(trail->lst, sizeof(Component));
	temp->totalValue = trail->totalValue ;
	temp->totalWeight = trail->totalWeight ;
	temp->fitness = trail->fitness ;

	return temp ;
}

/* prints a component */
void dumpComponent(void *data)
{
	Component *c ;
	c = (Component*)data ;
	printf("{[ID:%d][v:%.2f][w:%.2f][p:%.2f][d:%.2f][u:%d][eta:%.2f]}\n", 
		c->ID, c->value, c->weight, c->p, c->d, c->u, c->eta);
}

/* adds a component to the trail */
void addComponent(Trail *trail, Component *c)
{
	pushFront(trail->lst, (void*)c, sizeof(Component));
	trail->lst->length++ ;
	trail->totalValue += c->value ;
	trail->totalWeight += c->weight ;
}

/* removes a component from a trail */
void removeComponent(Trail *trail)
{
	Component *c ;
	c = (Component*)popFront(trail->lst, sizeof(Component));
	trail->lst->length-- ;
	trail->totalValue -= c->value ;
	trail->totalWeight -= c->weight ;
	free(c);
}

/* a component comparator, checks if two components are same */
int isSame( void *data1, void *data2)
{
	Component *c1, *c2 ;
	c1 = (Component*)data1 ;
	c2 = (Component*)data2 ;
	if( (c1->ID == c2->ID) 
		&& (c1->value == c2->value) 
		&& (c1->weight == c2->weight))
		return TRUE ;
	else
		return FALSE ;
}

/* prints a trail */
void dumpTrail(void *data)
{
	Trail *trail ;
	trail = (Trail*)data ;
	dump(trail->lst, dumpComponent);
	printf("\t\t\t--> V:%.2f, W:%.2f, f:%.2f\n", 
		trail->totalValue, trail->totalWeight, trail->fitness);
}

/**
	Initial random walk starting from each component.
**/
Trail* initialRandomWalk(KnapsackDescription *kpdsc)
{
	int i, index ;
	Trail *trail ;
	List *MasterList, *NeighbourList ;
	Component *c ;
	
	trail = newTrail();

	/* step 1: create a master list of 
		   components from the knapsack description 
	*/
	MasterList = newList();
	for( i = 0 ; i < kpdsc->length ; i++)
		pushFront(MasterList, (void*)&kpdsc->c[i], sizeof(Component));
	
	/* step 2: pick a random component c from MasterList */
	index = tossIntRandom(0, MasterList->length-1);
	c = (Component*)pickNode(MasterList, index, sizeof(Component));
	
	while(1)
	{
		/* step 3: add it to the front of the trail */
		addComponent(trail, c);
		/* step 4: check if the trail exceeds capacity
		           if yes, remove it from the trail
		           if no, keep it
		*/
		if( trail->totalWeight > kpdsc->capacity + LATENCY)
			removeComponent(trail);

		/* current component is used, remove it from the master list */
		pluckNode(MasterList, (void*)c, sizeof(Component), isSame);

		/* step 5: if all components are used, exit the procedure */
		if( MasterList->length == 0 )
			break ;
		else
		{
			/* step 6: create a list of c's neghbours from the master list */
			NeighbourList = createNeighbourList(c, MasterList, kpdsc);
			free(c);
			/* step 7: pick a random component c' from neighbour list */
			index = tossIntRandom(0, NeighbourList->length-1);
			c = (Component*)pickNode(NeighbourList, index, sizeof(Component));
			/* step 8: discard the list N */
			freeList(NeighbourList);
			/* step 9: go to step 3 */
		}
	}
	free(c);
	freeList(MasterList);
	
	return trail ;
}

/**
	creates a list composed of neighbourhood of 
	component c, where NeighbourList is a subset of MasterList	
**/
List* createNeighbourList(Component *c, List *MasterList, KnapsackDescription *kpdsc)
{
	int row, col ;
	List *NeighbourList ;
	ListIterator *iter ;
	Component *k ;

	if( isEmpty(MasterList) )
	{
		fprintf(stderr, "Master List is Empty!\n");
		exit(1);
	}
	else
	{
		NeighbourList = newList();
		row = c->ID ;

		iter = newListIterator(MasterList);
		while( hasNext(iter) )
		{
			k = (Component*)getCurrent(iter, sizeof(Component)) ;
			col = k->ID ;
			if(kpdsc->adj.e[row][col] == ALLOWED)
				pushFront(NeighbourList, (void*)&kpdsc->c[col], sizeof(Component));
			next(iter);
			free(k);
		}
		free(iter);
	}

	return NeighbourList ;
}

/* calculates fitness */
void calculateFitness(Trail *trail, KnapsackDescription *kpdsc)
{
	trail->fitness = trail->totalValue ;
}

/* constructs a solution using tournament selection */
Trail* constructSolutionUsingSizedTournament(KnapsackDescription *kpdsc, double tsize)
{
	int i ;
	Trail *trail ;
	List *MasterList, *NeighbourList ;
	Component *c;
	
	trail = newTrail();

	/* step 1: create a master list of 
		   components from the knapsack description 
	*/
	MasterList = newList();
	for( i = 0 ; i < kpdsc->length ; i++)
		pushFront(MasterList, (void*)&kpdsc->c[i], sizeof(Component));

	/* step 2: pick a component c from the MasterList using tournament */
	c = tournamentSelect(MasterList, tsize);

	while(1)
	{
		/* step 3: add it to the front of the trail */
		addComponent(trail, c);
		/* step 4: check if the trail exceeds capacity
		           if yes, remove it from the trail
		           if no, keep it
		*/
		if( trail->totalWeight > kpdsc->capacity + LATENCY)
			removeComponent(trail);
		/* component referenced by c is used, remove it from the master list */
		pluckNode(MasterList, (void*)c, sizeof(Component), isSame);

		/* step 5: if all components are used, exit the procedure */
		if( MasterList->length == 0 )
			break ;
		else
		{
			/* step 6: create a list of c's neghbours from the master list */
			NeighbourList = createNeighbourList(c, MasterList, kpdsc);
			free(c);
			/* step 7: pick a component c from the NeighbourList using tournament */
			c = tournamentSelect(NeighbourList, tsize);
			/* step 8: discard the list N */
			freeList(NeighbourList);
			/* step 9: go to step 3 */
		}
	}
	free(c);	
	freeList(MasterList);
	
	return trail ;
}

/* Tournament selection procedure */
Component* tournamentSelect(List *list, double size)
{
	int i, index ;
	double r, prob ;
	int usize, lsize ;
	List *origList, *bigList, *smallList ;
	Component *c1, *c2, *c ;

	bigList = newList(); 
	smallList = newList();
	
	lsize = floor(size) ;
	usize = ceil(size) ;
	prob = size - lsize ;

	/* if the length of the original list 
	   is smaller than the torunament size,
	   do a traditional tournament.
	*/
	if( list->length == 1 )
		c = (Component*)popFront(list, sizeof(Component));
	else if( list->length == 2 )
	{
		origList = cloneList(list, sizeof(Component));
		index = tossIntRandom(0, origList->length-1);
		c = (Component*)pickNode(origList, index, sizeof(Component));
		pluckNode(origList, c, sizeof(Component), isSame);
		freeList(origList);
	}
	else if( list->length <= lsize && list->length > 2)
	{
		origList = cloneList(list, sizeof(Component));
		index = tossIntRandom(0, origList->length-1);
		c1 = (Component*)pickNode(origList, index, sizeof(Component));
		pluckNode(origList, c1, sizeof(Component), isSame);
		index = tossIntRandom(0, origList->length-1);
		c2 = (Component*)pickNode(origList, index, sizeof(Component));
		pluckNode(origList, c2, sizeof(Component), isSame);
		c = tournament(*c1, *c2);
		freeList(origList);
		free(c1); free(c2);
	}
	else
	{
		origList = cloneList(list, sizeof(Component));
		for( i = 0 ; i < lsize ; i++ )
		{
			index = tossIntRandom(0, origList->length-1);		
			c = (Component*)pickNode(origList, index, sizeof(Component));
			pushFront(smallList, c, sizeof(Component));
			pluckNode(origList, c, sizeof(Component), isSame);
			free(c);
		}
		freeList(origList);
	
		origList = cloneList(list, sizeof(Component));
		for( i = 0 ; i < usize ; i++ )
		{
			index = tossIntRandom(0, origList->length-1);		
			c = (Component*)pickNode(origList, index, sizeof(Component));
			pushFront(bigList, c, sizeof(Component));
			pluckNode(origList, c, sizeof(Component), isSame);
			free(c);
		}
		freeList(origList);
	
		r = tossRealRandom(0.0, 1.0);
		if( r <= prob )
		{
			index = tossIntRandom(0,bigList->length - 1);
			c1 = (Component*)pickNode(bigList, index, sizeof(Component));
			index = tossIntRandom(0, bigList->length -1);
			c2 = (Component*)pickNode(bigList, index, sizeof(Component));
		}
		else
		{
			index = tossIntRandom(0,smallList->length - 1);
			c1 = (Component*)pickNode(smallList, index, sizeof(Component));
			index = tossIntRandom(0, smallList->length - 1);
			c2 = (Component*)pickNode(smallList, index, sizeof(Component));
		}
		c = tournament(*c1, *c2);
		free(c1); free(c2);
	}

	freeList(smallList);
	freeList(bigList);
	
	return c ;
}

Trail* constructSolutionUsingSimpleTournament(KnapsackDescription *kpdsc)
{
	int i, index ;
	Trail *trail ;
	List *MasterList, *NeighbourList ;
	Component *c, *c1, *c2 ;
	
	trail = newTrail();

	/* step 1: create a master list of 
		   components from the knapsack description 
	*/
	MasterList = newList();
	for( i = 0 ; i < kpdsc->length ; i++)
		pushFront(MasterList, (void*)&kpdsc->c[i], sizeof(Component));

	/* step 2: pick a component c from the MasterList using tournament */
	index = tossIntRandom(0, MasterList->length-1);
	c1 = (Component*)pickNode(MasterList, index, sizeof(Component));
	index = tossIntRandom(0, MasterList->length-1);
	c2 = (Component*)pickNode(MasterList, index, sizeof(Component));
	c = tournament(*c1, *c2);
	free(c1); free(c2);

	while(1)
	{
		/* step 3: add it to the front of the trail */
		addComponent(trail, c);
		/* step 4: check if the trail exceeds capacity
		           if yes, remove it from the trail
		           if no, keep it
		*/
		if( trail->totalWeight > kpdsc->capacity + LATENCY)
			removeComponent(trail);
		/* component referenced by c is used, remove it from the master list */
		pluckNode(MasterList, (void*)c, sizeof(Component), isSame);

		/* step 5: if all components are used, exit the procedure */
		if( MasterList->length == 0 )
			break ;
		else
		{
			/* step 6: create a list of c's neghbours from the master list */
			NeighbourList = createNeighbourList(c, MasterList, kpdsc);
			free(c);
			/* step 7: pick a component c from the NeighbourList using tournament */
			index = tossIntRandom(0, NeighbourList->length-1);
			c1 = pickNode(NeighbourList, index, sizeof(Component));
			index = tossIntRandom(0, NeighbourList->length-1);
			c2 = pickNode(NeighbourList, index, sizeof(Component));
			c = tournament(*c1, *c2);
			free(c1); free(c2);
			/* step 8: discard the list N */
			freeList(NeighbourList);
			/* step 9: go to step 3 */
		}
	}
	free(c);	
	freeList(MasterList);
	
	return trail ;
}

Trail* constructGreedySolution( KnapsackDescription *kpdsc)
{
	int i ;
	Trail *temp ;
	temp = newTrail();
	for( i = 0 ; i < kpdsc->length ; i++ )
		addComponent(temp, &kpdsc->c[i]);
	heapSortNaive(temp->lst, compareEta);

	dumpTrail(temp);

	return temp ;
}

int compareEta(void *d1, void *d2)
{
	Component *c1, *c2 ;
		
	c1 = (Component*)d1 ;
	c2 = (Component*)d2 ;

	if( c1->eta >= c2->eta )
		return TRUE ;
	else
		return FALSE ;
}

Component* tournament(Component c1, Component c2)
{
	Component *c ;
	double r ;

	c = malloc(sizeof(Component));

	if( c1.u < m && c2.u >= m )
		*c = c1 ;
	else if( c2.u < m && c1.u >= m)
		*c = c2 ;
	else if( c1.p > c2.p )
		*c = c1 ;
	else if( c2.p > c1.p )
		*c = c2 ;
	else if(c1.u < c2.u )
		*c = c1 ;
	else if(c2.u < c1.u )
		*c = c2 ;
	else
	{
		r = tossRealRandom(0.0, 1.0) ;
		if( r < 0.5 )
			*c = c1 ;
		else
			*c = c2 ;
	}

	return c ;
}

/* checks if a component c is used in a trail */
int isUsed(Component *c, Trail *trail)
{
	int ret ;
	ListIterator *iter ;
	Component *k ;

	ret = FALSE ;
	iter = newListIterator(trail->lst);
	do
	{
		k = (Component*)getCurrent(iter, sizeof(Component)) ;
		if( isSame((void*)c, (void*)k) )
		{
			ret = TRUE ;
			free(k);
			break ;
		}
		next(iter);
		free(k);
	}
	while(hasNext(iter));
	free(iter);
	
	return ret ;
}

/* finds the max between a and b */
double max(double a, double b)
{
	if( a >= b )
		return a ;
	else
		return b ;
}

