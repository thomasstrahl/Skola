#include <signal.h>
#include <stdio.h>



void user2()
{

	printf("user2!!\n");
}

void
termination_handler (int signum)
{
	struct sigaction new_action;
	struct sigaction usr_2;

  /* Set up the structure to specify the new action. */
	new_action.sa_handler = SIG_IGN;
	sigemptyset (&new_action.sa_mask);
	sigaddset(&new_action.sa_mask, SIGUSR1);
	new_action.sa_flags = 0;
	sigaction(SIGUSR1, &new_action, 0);

	usr_2.sa_handler = user2;
	sigemptyset(&usr_2.sa_mask);
	sigaddset(&usr_2.sa_mask, SIGUSR2);
	usr_2.sa_flags = 0;
	sigaction(SIGUSR2, &usr_2, 0);
	printf("termination handler\n");
	for (;;);
		
}

int
main (void)
{
	struct sigaction new_action;

  /* Set up the structure to specify the new action. */
	new_action.sa_handler = termination_handler;
	sigemptyset (&new_action.sa_mask);
	new_action.sa_flags = 0;
	sigaction(SIGINT, &new_action, 0);
	printf("main\n");
	for(;;);
}