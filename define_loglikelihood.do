* This program defines the likelihood functions of the RPM model to be used by the ML command

* Logit Model
program define loglike_RPM_logit

    * Define output (lnf) and input arguments of the function
    args lnf r LNlambda kapp

    * Define temporal variables used in the program
    tempvar choice omega lambda ps

    * Do the following without showing output
    quietly {

        * Transform  model parameters to restrict their domains
        generate double `lambda' = exp(`LNlambda')  // Restrict lambda to be positive 
        
        * Create the following variables using input of the function
        generate double `choice'   = $ML_y1    // Chosen lottery
        generate double `omega'    = $ML_y2    // Risk aversion level that equates EU of lotteries
        
        * Compute probability of choosing lottery X (0) over lottery  Y (1)
        generate double `ps' = (1-2*`kapp')*invlogit( (`r'-`omega')*`lambda' ) + `kapp'

        * Compute log-likelihood depending on choice of lottery
        replace `lnf' = ln(`ps')                   if `choice' == 0   // If chooses X                       
        replace `lnf' = ln(1-`ps')                 if `choice' == 1   // If chooses Y               
        replace `lnf' = 0.5*(ln(`ps')+ln(1-`ps'))  if `choice' == -1  // If is indifferent

    }

end

********************************************************************************************

* Probit Model
program define loglike_RPM_probit

    * Define output (lnf) and input arguments of the function
    args lnf r LNlambda kapp

    * Define temporal variables used in the program
    tempvar choice omega lambda ps

    * Do the following without showing output
    quietly {

        * Transform  model parameters to restrict their domains
        generate double `lambda' = exp(`LNlambda')  // Restrict lambda to be positive         

        * Create the following variables using input of the function
        generate double `choice'   = $ML_y1  // Chosen lottery
        generate double `omega'    = $ML_y2  // Risk aversion level that equates EU of lotteries
        
        * Compute probability of choosing lottery X (0) over lottery  Y (1)
        generate double `ps' = (1-2*`kapp')*normal( (`r'-`omega')*`lambda' ) + `kapp'

        * Compute log-likelihood depending on choice of lottery
        replace `lnf' = ln(`ps')                   if `choice' == 0    // If chooses X                       
        replace `lnf' = ln(1-`ps')                 if `choice' == 1    // If chooses Y                        
        replace `lnf' = 0.5*(ln(`ps')+ln(1-`ps'))  if `choice' == -1   // If is indifferent

    }

end
