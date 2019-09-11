#'
#' transform a date in julian day
#' 
#' @export
#' 
#' @param datStr, eg :  "2014-01-01"
#'   
#' @return the date in julian day: 2456659
#' 
#' @author Ronan Trépos MIA-T, INRA
#' 
#' @note

rvleExp.dateToNum = function(dateStr)
{
  if (! is.character(dateStr)) {
    return(NA);
  } 
  return (as.numeric(as.Date(dateStr, format="%Y-%m-%d") + 2440588));
}


#'
#' transform a date of the form 2456659 into an object Date
#' 
#' @export
#' 
#' @param dateNum, eg :  2456659
#' @param round, are numerical rounded 
#'   
#' @return return a date object, eg:  as.Date("2014-01-01")
#' 
#' @author Ronan Trépos MIA-T, INRA
#' 
#' @note
#' 
rvleExp.dateFromNum = function(dateNum, round=T)
{
  if (round) {
    return(as.Date(as.Date(round(dateNum), origin="1970-01-01") - 2440588));
  } else {
    return(as.Date(as.Date(dateNum, origin="1970-01-01") - 2440588));
  }
}

#'
#' split an input id of the form cond.port to con and port.
#' 
#' @export
#' 
#' @param id, eg :  cond.port
#'   
#' @return return a list containing cond and port
#' 
#' @author Ronan Trépos MIA-T, INRA
#' 
#' @note
#' 
rvleExp.idToCondPort = function(id)
{
  resSPlit = strsplit(id, split="\\.")[[1]];
  if (length(resSPlit) != 2) {
    stop(paste("[rvleExp] Error: cond.port malformed", id));
  }
  return(list(cond=resSPlit[1], port=resSPlit[2]));
}

#'
#' Set outputs for simulation
#' 
#' @export
#' 
#' @param vleObj, a rvle handle built with rvle.open
#' @param output_vars, a list of named characher eg. 
#'          c(portname="view/Coupled:atomic.port)
#'        each one identifies an output, names are output ids and content are paths
#' @param integration, list of integrations type amongst 'last', 'max'
#'        or 'all' (default = 'last') of size length(output_vars)
#' @param aggregation_input, list of aggregation types amongst 'mean', 'max' 
#'        or 'all' (default = 'all') of size length(output_vars)
#' 
#' @return NULL
#' 
#' @author Ronan Trépos MIA-T, INRA
#' 
#' @note
#' 
#' all configurations cannot be given with this function (aggregation_replicate, mse,
#' quantile)
#' 
#' @examples
#' 
#' #TODO
#' 
rvleExp.configOutputs = function(vleObj=NULL, output_vars=NULL,  
                                 integration=NULL, aggregation_input=NULL)
{
  if (sum(duplicated(names(output_vars))) > 0){
    stop(paste(sep="", "[rvleExp] Error: duplicated output:'",
               names(output_vars)[which(duplicated(names(output_vars)))[1]],
               "'"));
    return (NULL);
  }
  if (is.null(integration)) {
    integration = rep('last', length(output_vars));
  } else if (length(integration) == 1) {
    integration = rep(integration, length(output_vars));
  }
  if (is.null(aggregation_input)) {
    aggregation_input = rep('all', length(output_vars));
  } else if (length(aggregation_input) == 1) {
    aggregation_input = rep(aggregation_input, length(output_vars));
  }
  i = 0;
  for (var in names(output_vars)) {
    i = i+1;
    rvle.plan_output(vleObj=vleObj, id=var,
                    path=output_vars[[var]], integration=integration[i], 
                    aggregation_input=aggregation_input[i]);
  } 
}

#'
#' Parses a simulation file
#'
#' @param file_sim, either a filename or a dataframe providing bounds on
#'        inputs. Columns are of type 'cond.port', lines are 'default', 
#'        'min' and 'max'.
#' @param vleObj [optionnal], a rvle handle built with
#'        rvle.open, input values are used to initialize the model
#' @param id [default:NULL], id of simulations to keep
#' @param fillMissing [default:depends on vleObj], if TRUE, then 
#'        the simulations with missing value are filled with the default value 
#'        in the original vpz.
#' @param withWarnings [default:TRUE], gives warnings if true
#' @param sep [default:";"], separator type for columns in file
#' @param skip [default:1], skip parameter of read.table
#' @return the dataframe of intputs
#'
#' usage:
#'
#'  
#'
rvleExp.parseSim = function(file_sim=NULL, vleObj=NULL, id=NULL,
  fillMissing = ! is.null(vleObj), withWarnings=TRUE, sep=";", skip=1)
{
  #read inputs
  if (is.character(file_sim)) {
    file_sim = utils::read.table(file_sim, sep=sep, skip=skip, header=TRUE, 
                          stringsAsFactors = FALSE); 
  }
  
  #remove useless columns
  headerNames = names(file_sim)
  for (n in headerNames) {
    if (substring(n,1,1) == "X") { #remove it
      file_sim[[n]] <- NULL;
    } else if (all(is.na(file_sim[[n]]))){
      file_sim[[n]] <- NULL;
      if (withWarnings) {
        warning(paste("Warning in simulation data: Na in all column", n, 
                      ", column is removed"));  
      }
    } else if (sum(is.na(file_sim[[n]])) > 0) {
      if (withWarnings) {
        strId = paste(which(is.na(file_sim[[n]])), collapse=",");
        treatment = 'removed';
        if (fillMissing) {
          treatment = 'filled with default value';
        }
        warning(paste("Warning  in simulation data: Na in column", n,
                      ", missing lines are ",treatment," : id=", 
                      strId));
      }
      if (fillMissing) {
        file_sim[is.na(file_sim[[n]]),n] <- 
          rvle.get_condition_port_value(vleObj, 
                                      rvleExp.idToCondPort(n)$cond, 
                                      rvleExp.idToCondPort(n)$port);
      } else {
        file_sim = file_sim[!is.na(file_sim[[n]]), ];
      }
    }
  }
  
  #keep only line to simulate
  if (! is.null(id)) {
    if (! all(id %in% file_sim$id) & withWarnings) {
      warning("[rvleExp] requiring to simulate id that does not exist");
    }
    file_sim = file_sim[match(id, file_sim$id),];
  }
  
  #config inputs to simulate
  if (! is.null(vleObj)) {
    for (input in names(file_sim)) {
      if (input != "id") {
        if (nrow(file_sim) > 1) {
          rvle.plan_input(vleObj, rvleExp.idToCondPort(input)$cond, 
                         rvleExp.idToCondPort(input)$port, 
                         val=file_sim[[input]]);
        } else {
          rvle.plan_propagate(vleObj, rvleExp.idToCondPort(input)$cond, 
                             rvleExp.idToCondPort(input)$port, 
                             val=file_sim[[input]]);
        }
      }
    }
  }
  
  # force numeric values for duration (expected by vle kernel)
  if ('simulation_engine.duration' %in% colnames(file_sim)) {
    file_sim$simulation_engine.duration = 
      as.numeric(file_sim$simulation_engine.duration)
  }
  
  return(file_sim)
}


#'
#' Parses an observation file
#'
#' @export
#' 
#' @param file_obs,  filename of observations
#' @param vleObj [optionnal], a rvle handle built with
#'        rvle.open, input values are used to initialize the model
#' @param id [default:NULL], id of observations to keep
#' @param withWarnings [default:TRUE], gives warnings if true
#' @param skip [default:1], skip parameter of read.table
#' 
#' @return a dataframe of observations whit attributes giving the paths
#'         to atomic ports on views
#' 
rvleExp.parseObs = function(file_obs=NULL, vleObj=NULL, id=NULL,
                                  withWarnings=TRUE, skip=1)
{
  #reader header
  header = NULL;
  if (is.character(file_obs)) {#a file name
    header = utils::read.table(file_obs, sep=";", skip=skip, header=TRUE, 
                               nrow=1, stringsAsFactors = FALSE);
    #remove 'X\\..*'
    toremove = grep ("X\\.", names(header));
    #remove 'X'
    toremove = c(toremove, which(names(header) == "X"));
    if (length(toremove) > 0) {
      header = header[,-toremove];
    }
    header = as.list(header);
    file_obs = utils::read.table(file_obs, sep=";", skip=2+skip, header=FALSE, 
                          stringsAsFactors = FALSE);
    if (length(toremove) > 0) {
      file_obs = file_obs[,-toremove];
    }
    colnames(file_obs) <- names(header);
    attr(file_obs, "paths")<-header;
  } else {#a data frame
    if (is.null(attr(file_obs, "paths"))){
      if (! is.null(vleObj)){
        stop("[rvleExp] missing output paths to configure model");
      }
      header_tmp = lapply(names(file_obs), function(x){
        "unknown vle variable"});
      names(header_tmp) <- names(file_obs) 
      attr(file_obs,"paths") <-header_tmp ;
      
    }
    header = attr(file_obs, "paths")[names(file_obs)];
    header = header[which(!is.na(names(header)))]
  }
  
  #keep only line to simulate
  if (! is.null(id)) {
    if (! all(id %in% file_obs$id) & withWarnings) {
      warning(paste("[rvleExp] requiring to keep observations",
                    "that does not exist"));
    } 
    file_obs = file_obs[which(is.element(file_obs$id, id)),];
  }
  
  #remove useless columns and lines of file_obs
  for (n in names(header)) {
    if (all(is.na(file_obs[[n]]))){
      file_obs[[n]] <- NULL;
      if (withWarnings){
        attr(file_obs,"paths")[[n]] <- NULL
        warning(paste("remove column because all are NA:", n))
      }
    }
  }
  toremove = NULL;
  for (r in 1:nrow(file_obs)) {
    if (all(is.na(file_obs[r,]))) {
      toremove = c(toremove, r);
      if (withWarnings){
        warning(paste("remove row because all are NA:", r))
      }
    }
  }
  if (! is.null(toremove)) {
    file_obs = file_obs[-toremove,];    
  }
  
  #config output
  if (! is.null(vleObj)) {
    header[["id"]]<-NULL;
    rvleExp.configOutputs(vleObj=vleObj,
                          output_vars=header, integration='all');
  }
  return (file_obs);
}

#'
#' Parses an experiment configuration file
#'
#' @param file_expe, either a filename or a dataframe providing bounds on
#'        inputs. Columns are of type 'cond.port', lines are 'default', 
#'        'min' and 'max'.
#' @param vleObj [optionnal], a rvle handle built with
#'        rvle.open, default values are used to initialize the model
#' @param typeReturn [default:'all'], either 'all' or 'bounds'
#'        it defines the obect to return
#' @param skip [default:0], skip parameter of read.table
#' @return either:
#'    - the bounds min max of the experiment if typeReturn == 'bounds'
#'    - the all dataframe (with default values) if typeReturn == 'all'
#'
#' usage:
#'  
#'
rvleExp.parseExpe = function(file_expe=NULL, vleObj=NULL,
                                   typeReturn='all', skip=0)
{
  if (is.character(file_expe)) {
    file_expe = utils::read.table(file_expe, sep=";", header=T, 
                                  stringsAsFactors=F, skip=skip, 
                                  row.names=c("default", "min","max"));
  }
  expe = file_expe;
  expe$parameter <- NULL;
  bounds=NULL;
  bounds_col_names=NULL;
  #set propagate and define bounds
  for (i in 1:ncol(expe)) {
    #convert date to int if required
    if (! is.na(rvleExp.dateToNum(expe["default", i]))) {
      if (is.na(expe["min",i])) {
        expe[, i] = as.numeric(c(
          rvleExp.dateToNum(expe["default", i]), NA, NA));
      } else {    
        expe[, i] = as.numeric(c(
          rvleExp.dateToNum(expe["default", i]), 
          rvleExp.dateToNum(expe["min", i]),
          rvleExp.dateToNum(expe["max", i])));
      }
    }
    #define propagate
    if (! is.null(vleObj)) {
      rvle.plan_propagate(vleObj=vleObj, 
                         cond = rvleExp.idToCondPort(names(expe)[i])$cond, 
                         port = rvleExp.idToCondPort(names(expe)[i])$port, 
                         val=expe["default",i]);
    }
    if (! is.na(expe["min",i])) {
      inputName = names(expe)[i];
      #define bound
      bounds = cbind(bounds, c(expe["min",i], expe["max",i])); 
      bounds_col_names = c(bounds_col_names, inputName);
    }
  }
  bounds = data.frame(bounds, row.names=c("min", "max"));
  colnames(bounds) <- bounds_col_names
  if (typeReturn == 'all') {
    return (file_expe);
  } else {
    return (bounds);
  }
}

#'
#' Performs a sensitivity analysis
#'
#' @param vleObj, a rvle handle built with rvle.open
#' @param file_expe, a file_expe parameter of rvleExp.parseExpe
#' @param method, either 'morris' or 'fast99' or 'sobolEff'
#' @param r, number of replicate of the morris method
#' @param levels, number levels of the morris method
#' @param n, sample size of fast99 method or sobolEff
#' @param output_vars [optionnal], these are the output variables for which 
#'  sensitivity indices should be computed and that are not directly available
#'  from simulation results. If not provided, sensisitivy indices are computed 
#'  for all simulated variables, taking the last value available
#' @param handleY [optionnal], this function must return the vector of 
#'  simulated variables for the sensitivity experiment plan. Its arguments 
#'  are the name of the output variable and the vle simulation results of the
#'  experiment plan.
#' @param typeReturn, either 'indices', 'out' or NULL . If NULL, both the
#'  indices and simulation results are returned (default : NULL)
#' 
#' usage:
#'  
#'
rvleExp.sensitivity = function(vleObj=vleObj, file_expe=NULL,
                               method='morris', r=100, levels=5, n=100, 
                               output_vars=NULL, handleY=NULL, typeReturn=NULL)
{
  requireNamespace("sensitivity")
  
  #provide a default handleY function
  if (is.null(handleY)) {
    handleY = function(output_var, vle_res) {
      return(vle_res[[output_var]][nrow(vle_res[[output_var]]),])
    }
  }
  
  #read bounds
  bounds = rvleExp.parseExpe(file_expe, vleObj, typeReturn='bounds');
  
  #generate plan
  sensi_plan = NULL;
  if (method == 'morris') {
    sensi_plan = sensitivity::morris(model=NULL, 
                    factors = as.character(colnames(bounds)), 
                    scale=TRUE, #warning!! this is required, see if one could 
                                # directly simulate into [0;1]
                    r = r, design =list(type="oat", levels=levels, grid.jump=1),
                    binf=as.numeric(bounds["min",]), 
                    bsup=as.numeric(bounds["max",]));
  } else if (method == 'fast99'){
    bounds_f = NULL
    for (i in 1:ncol(bounds)) {
      bounds_f[[i]] = list(min=bounds["min",i], max=bounds["max",i])
    }
    
    sensi_plan = sensitivity::fast99(model = NULL, factors=names(bounds), n=n, 
                                     q="qunif",
                        q.arg = bounds_f);
    if (sum(is.na(sensi_plan$omega)) > 1){
      stop("[rvleExp] Error: fast99 not enough sims (?)");
    }
  } else if (method == 'sobolEff'){
    X1 = matrix(stats::runif(ncol(bounds) * n), nrow = n);
    X2 = matrix(stats::runif(ncol(bounds) * n), nrow = n);
    for (i in 1:ncol(bounds)) {
      X1[,i] = bounds["min",i] + X1[,i] * (bounds["max",i]- bounds["min",i]);
      X2[,i] = bounds["min",i] + X2[,i] * (bounds["max",i]- bounds["min",i]);
    }
    X1 = as.data.frame(X1);
    X2 = as.data.frame(X2);
    colnames(X1) <- colnames(bounds);
    colnames(X2) <- colnames(bounds);
    sensi_plan = sensitivity::sobolEff(model = NULL, X1=X1, X2=X2);
  }
  print(paste('will perform', nrow(sensi_plan$X),'simulations'))
  
  #config simulator with exp plan
  for (i in 1:ncol(bounds)){
    rvle.plan_input(vleObj=vleObj, 
                    cond = rvleExp.idToCondPort(colnames(bounds)[i])$cond, 
                    port = rvleExp.idToCondPort(colnames(bounds)[i])$port, 
                    val=sensi_plan$X[,i]);
  }
  vle_res = rvle.plan_run(vleObj);
  
  #provide a default list of output varaibles 
  if (is.null(output_vars)){
    output_vars = names(vle_res);
  }
  
  res_sensitivity = list();
  for (output_var in output_vars) {
    sensi_plan_bis = sensi_plan;
    sensitivity::tell(sensi_plan_bis, handleY(output_var, vle_res));
    res_sensitivity[[output_var]] <- sensi_plan_bis
  }
  if (is.null(typeReturn)){
    return(list(indices=res_sensitivity, out=vle_res))  
  } else if (typeReturn == 'out') {
    return(list(out=vle_res))  
  } else {
    return(list(indices=res_sensitivity))  
  }
}

#'
#' Performs MCMC estimation
#'
#' @param vleObj, a rvle handle built with rvle.open
#' @param file_expe, a file_expe parameter of rvleExp.parseExpe
#' @param intern_like [optionnal], define the likelihood function 
#'            (if NULL, a default one is defined)
#' @param n [default:1000], total number of simulations
#' @param startValues [default:4], number of chains in the MCMCMC.
#'   number of simulations by chain is n/startValue
#'
#' usage:
#'
rvleExp.mcmc = function(vleObj=NULL, file_expe=NULL,
                              intern_like=NULL, n=1000, startValue=4)
{
  requireNamespace("BayesianTools")
  file_expe = rvleExp.parseExpe(file_expe, vleObj,
                                      typeReturn='bounds');
  #define intern optim fun
  if (is.null(intern_like)) {
    intern_like = function(x) {
      if (! is.matrix(x)){
        x = t(as.matrix(x))
      }
      if (ncol(x) != ncol(file_expe)){
        print(paste("[rvleExp] error cols "));
      }
      for (i in 1:ncol(file_expe)) {
        rvle.plan_input(vleObj, rvleExp.idToCondPort(names(file_expe)[i])$cond,
                        rvleExp.idToCondPort(names(file_expe)[i])$port, x[,i]);
      }
      r = rvle.plan_run(vleObj);
      if ((length(r) != 1)||(dim(r[[1]])[1] != 1)||(dim(r[[1]])[2] != nrow(x))){
        print(paste("[rvleExp.mcmc] error ambiguity in output ",
                    length(r), dim(r[[1]])[1], dim(r[[1]])[2]))
      }
      return (- 0.5*r[[1]][1,]);
    }
  }
  bayesianSetup = BayesianTools::createBayesianSetup(likelihood=intern_like,
                                      lower=as.numeric(file_expe[1,]), 
                                      upper=as.numeric(file_expe[2,]),
                                      parallel = "external", 
                                      names=names(file_expe))
  res <- BayesianTools::runMCMC(bayesianSetup = bayesianSetup, 
                 sampler = "DREAM", settings = list(iterations = n, 
                                                    startValue = startValue))
  return(res);
}

#'
#' Performs mono-objective simulation optimization using 
#' genoud (from rgenoud package).
#' 
#' @param rvle_handle, a rvle handle built with rvle.open
#' @param file_expe, a file_expe parameter of rvleExp.parseExpe
#' @param pop.size, see genoud parameter (default : 4)
#' @param max.generations, see genoud parameter (default : 5)
#' @param optim.method, see genoud parameter (default : 'L-BFGS-B').
#'   if 'Nelder-Mead', then a default handleX funtion is given that handles 
#'   out-of-bounds X values (a problem in rgenoud?). The default is 'L-BFGS-B'
#'   since no out-of-bounds evaluation should be requested 
#'   (boundary.enforcement=2)
#' @param handleX [optionnal], function called on the X parameters before
#'   launching the simulation, if it returns NA then the simulation is called,
#'   otherwise the values return is directly the fitness value
#' @param handleY [optionnal], function called on the simulation outputs 
#'   (from rvle.plan_run) that computes the aggregation of output in a
#'   unique fitness value.
#'
#' usage:
#'
#'
rvleExp.optim = function(vleObj=NULL, file_expe=NULL, 
                         pop.size=4, max.generations=5, 
                         optim.method="L-BFGS-B",
                         handleX=NULL, handleY=NULL)
{
  requireNamespace("rgenoud")
  file_expe = rvleExp.parseExpe(file_expe, vleObj, typeReturn='bounds');
  
  #define default handleX if required 
  if ((optim.method == 'Nelder-Mead') & is.null(handleX) ) {
    handleX = function (x) {
      for (i in 1:ncol(file_expe)) {
        if ((file_expe[1,i] > x[i]) | (x[i] > file_expe[2,i])) return(9999999)
      }
      return(NA);
    }
  }
  
  #define default handleY the default y takes the first value available
  if (is.null(handleY) ) {
    handleY = function (vle_res) {
      if ((length(vle_res) != 1) || (dim(vle_res[[1]])[1] != 1) || 
          (dim(vle_res[[1]])[2] != 1)) {
        stop(paste("[rvleExp.optim] ambiguity in output "))
      }
      return (vle_res[[1]][1,1]);
    }
  }
  
  #define intern optim fun
  intern_fun = function(x, vleObj, file_expe, withSpawn, handleX, handleY){
    if (! is.null(handleX)){
      fitness = handleX(x);
      if (! is.na(fitness)) return(fitness)
    }
    for (i in 1:ncol(file_expe)) {
      rvleExp.idToCondPort(names(file_expe)[i])
      rvle.plan_propagate(vleObj,
                         cond = rvleExp.idToCondPort(names(file_expe)[i])$cond,
                         port = rvleExp.idToCondPort(names(file_expe)[i])$port,
                         val=x[i]);
    }
    vle_res = rvle.plan_run(vleObj);
    return (handleY(vle_res));
  }
  
  res = rgenoud::genoud(fn=intern_fun, nvars=ncol(file_expe), pop.size=pop.size,
               max.generations=max.generations, Domains=t(as.matrix(file_expe)),
               boundary.enforcement=2, optim.method = optim.method,
               vleObj=vleObj, file_expe=file_expe,          #for internal fun
               handleX=handleX, handleY=handleY)            #for internal fun
  return(res);
}

#'
#' Extract simulation results form the overall structure
#' 
#'  @param res, results from rvle.plan_run
#'  @param time_ind, indices of time to extract from results
#'  @param date, dates at which to extract results
#'  @param file_sim, file of simulations
#'  @param id, indices of simulations to extract if file_sim is null,
#'             id of simulations to extrat otherwise
#'  @param output_vars, list of char giving the outputs to extract
#'  @param withWarnings, if true, print warnings
#'  @return a sub structure odf res
#' 
rvleExp.extract = function(res=NULL, time_ind=NULL, date=NULL,  
                           file_sim=NULL, id=NULL,  output_vars=NULL,
                           withWarnings=withWarnings)
{
  #get output vars
  if (is.null(output_vars)) {
    output_vars = names(res);
  }  else {
    output_vars = intersect(output_vars, names(res))
    if (length(output_vars) ==0 ) {
      stop(paste(sep="", "[rvleExp.extract] Error: output_vars
                 are not found in res"));
    }
    res = res[output_vars];
    }
  if ((!is.null(time_ind) | !is.null(date)) &
      !all(unlist(lapply(res, function(x) {nrow(x)})) == nrow(res[[1]]))) {
    stop(paste(sep="", "[rvleExp.extract] Error: all selected variables ",
               "do not have the same time indices ",
               "(use of 'time_ind' or 'date')"));
  }
  
  #get sim indices
  sim_ind = 1:ncol(res[[1]]);
  if (!is.null(file_sim)) {
    if (is.null(id)) {
      id = file_sim$id;
    }
    file_sim = rvleExp.parseSim(file_sim=file_sim, id = NULL,
                                      withWarnings=withWarnings);
    
    if (length(file_sim$id) != ncol(res[[1]])) {
      stop(paste(sep="", "[rvleExp.extract] Error: file_sim (",
                 length(file_sim$id)," ids) and res (", ncol(res[[1]]),
                 " sims) do not match"));
    }
    sim_ind = match(intersect(file_sim$id, id), file_sim$id);
  } else if (! is.null(id)){
    sim_ind = id;
  }
  
  #get time indices
  if (!is.null(date)) {
    if (!is.null(time_ind)){
      stop(paste("[rvleExp.extract] Error: cannot get",
                 "both dates and time_ind"));
    }
    if (is.character(date)){
      date = rvleExp.dateToNum(date);
    }
    if(!("date" %in% names(res))){
      stop(paste("[rvleExp.extract] Error: missing 'date' in results"));
    }
    time_ind = match(round(intersect(round(date),
                                     round(res$date[,sim_ind[1]]))),
                     round(res$date[,sim_ind[1]]));
    if (length(time_ind) == 0) {
      stop(paste(sep="", "[rvleExp.extract] Error: selected dates do ",
                 "not match simulation results"));
    }
  }
  #select final matrices
  for (var in names(res)) {
    if (is.null(time_ind)) {
      tmp_data_var = res[[var]];
      if (nrow(tmp_data_var) == 1) {#handle weird behavior of R
        res[[var]] <- as.matrix(t(tmp_data_var[, sim_ind]));
      } else {
        res[[var]] <- as.matrix(tmp_data_var[, sim_ind]);
      }
    } else {
      tmp_data_var = res[[var]];
      if (length(time_ind) == 1) {#handle weird behavior of R
        res[[var]] <- as.matrix(t(tmp_data_var[time_ind, sim_ind]));
      } else {
        res[[var]] <- as.matrix(tmp_data_var[time_ind, sim_ind]);
      }
    }
  }
  return(res)
}

#'
#' Align simulation and observations (in order to compute goodness of fit)
#' 
#'  @param res, simulation results
#'  @param file_sim, filename of simulations
#'  @param file_obs, filename of observations
#'  @param integration, temporal integration for all output variables 
#'                       ("all" or "max") 
#'                       eg : c(GY="max", LAI="all")
#'  @param id,  id to simulate for comparison
#'  @param withWarnings,  if true, print warnings
#'  @param print,  if true, print rmse
#'  @return a complex structure
#' 

rvleExp.compareSimObs=function(res=NULL, file_sim=NULL, file_obs=NULL,
                                     integration=NULL, id=NULL, 
                                     withWarnings=TRUE, print=FALSE)
{
  #read simulations
  file_sim = rvleExp.parseSim(file_sim=file_sim, withWarnings=withWarnings);
  
  #read observations
  file_obs = rvleExp.parseObs(file_obs=file_obs, id=id, 
                              withWarnings=withWarnings)
  if (is.null(integration)) {
    output_paths = attr(file_obs, "paths");
    output_paths[["id"]]<-NULL;
    output_paths[["date"]]<-NULL;
    integration = rep(list("all"), length(output_paths))
    names(integration) <- names(output_paths)
  }
  
  #check results
  if (is.null(res$date)) {
    stop("[rvleExp.compareSimObs] missing date in simulation results");
  }
  if (ncol(res$date) != nrow(file_sim)) {
    stop("[rvleExp.compareSimObs] simulation results do not 
         match simulation file");
  }
  
  #select subset of simulations
  isSim = 1:ncol(res[[1]]);
  if (!is.null(id)) {
    isSim = match(id, file_sim$id)
    if (length(isSim) != length(id)){
      stop("[rvleExp.compareSimObs] file_sim and id do not fit");
    }
    file_sim = rvleExp.parseSim(file_sim=file_sim, id=id, 
                                withWarnings=withWarnings);
  }
  
  #select subsets of results and reorder to fit id
  for (var in names(res)){
    res[[var]] = res[[var]][,isSim]
  }
  
  sim_vs_obs = NULL
  for (i in 1:length(isSim)) {
    #for (i in 1:1) {
    idi = file_sim$id[i]
    file_obs_i = file_obs[file_obs$id == idi,]
    for (var in names(integration)) {
      file_obs_i_var = file_obs_i[!is.na(file_obs_i[[var]]),]
      if (nrow(file_obs_i_var) > 0) {
        res_i_val = res[[var]][,i]
        if (integration[[var]] == "all") {
          #handle "all" integration
          res_i_date = round(res$date[,i])
          for (j in 1:nrow(file_obs_i_var)) {
            d_str = file_obs_i_var$date[j]
            d = rvleExp.dateToNum(d_str)
            if (! length(res_i_val[which(res_i_date == d)])) {
              stop(paste("[rvleExp.compareSimObs] no simulation result ",
                         "for variable '", var, "' for id '", idi, "' at date ",
                         d_str, sep=""))
            }
            sim_vs_obs = rbind(sim_vs_obs, 
                               data.frame(id=idi, date=d_str,
                                          output=var, 
                                          observed=file_obs_i_var[[var]][j], 
                                          simulated=
                                            res_i_val[which(res_i_date == d)], 
                                          stringsAsFactors=F))
          }
        } else if (integration[[var]] == "max") {
          #handle "max" integration
          sim_vs_obs = rbind(sim_vs_obs, 
                             data.frame(id=idi, date="max", 
                                        output=var, 
                                        observed=max(file_obs_i_var[[var]]),
                                        simulated=max(res_i_val),  
                                        stringsAsFactors=F))
        }
      }
    }
  }
  attr(sim_vs_obs, "rvle_obj") <- "rvleExp.compareSimObs"
  return(sim_vs_obs);
}

#'
#' Generic function for plot
#' 
#'  @param rvle_obj, results from rvle.plan_run
#'                    or rvleExp.compareSimObs
#'  @param file_sim, either 
#'         - a csv file name of simulations
#'         - a dataframe of simulations
#'  @param file_obs, either:
#'        - a csv file name of observations
#'        - a dataframe of observations
#'  @param output_vars, list of char giving the outputs to plot
#'  @param id, id of simulation or observations. is applied to selection in 
#'             file_sim and file_obs if not null (only one?)
#'  @param time_ind, time indices for plot 
#'                   (only for objects from rvle.plan_run)
#'  @param sim_legend, vector of char for legend of simulation
#'  @param typeReturn, either 
#'   - 'plot_list': the list of ggplot is returned (can be modified)
#'   - NULL: the ggplot are arranged and the return is NULL. if 'plot_list
#' 
rvleExp.plot = function(rvle_obj=NULL, file_sim=NULL, file_obs=NULL, 
                              output_vars=NULL,
                              id=NULL, time_ind=NULL,
                              sim_legend=NULL, typeReturn=NULL)
{
  requireNamespace("gridExtra")
  requireNamespace("grid")
  requireNamespace("ggplot2")
  requireNamespace("reshape2")
  
  gpAll = NULL;
  
  #build static plots
  if (attr(rvle_obj, "rvle_obj") == "rvleExp.compareSimObs"){
    if (! is.null(output_vars)) {
      rvle_obj = rvle_obj[rvle_obj$output %in% output_vars,]
    }
    if (is.null(file_sim)) {
      stop("[rvleExp.plot] file_sim is null");
    }
    if (is.null(file_obs)) {
      stop("[rvleExp.plot] file_obs is null");
    }
    i = 1;
    for (var in unique(rvle_obj$output)) {
      
      df = rvle_obj[rvle_obj$output == var,]
      print(paste("rmse ",var,":", 
                  sqrt(sum((df$observed-df$simulated)^2)/length(df$observed))))
      
      gpAll[[i]] = ggplot2::ggplot(data=df, ggplot2::aes_string(x="simulated", 
                                                                y="observed"));
      gpAll[[i]] = gpAll[[i]] + ggplot2::aes(colour=as.character(id))
      if (is.null(sim_legend)) {
        gpAll[[i]] = gpAll[[i]] + ggplot2::geom_text(ggplot2::aes(label=id)) +
          ggplot2::theme(legend.position="none");
      } else {
        gpAll[[i]] = gpAll[[i]] + ggplot2::geom_point()
      }
      gpAll[[i]] = gpAll[[i]] + ggplot2::geom_abline(intercept = 0, slope = 1) + 
        ggplot2::labs(x=paste(var, "sim"), y=paste(var, "obs")) ;
      i = i+1;
    }
  }
  
  #build dynamic plots
  if (attr(rvle_obj, "rvle_obj") == 'rvle.plan_run') {
    #identify output_vars and open obs
    if (is.null(output_vars)) {
      output_vars = names(rvle_obj);
    }
    if (! is.null(file_obs)) {
      file_obs = rvleExp.parseObs(file_obs=file_obs, id=id);
    }
    ##extract res
    res = rvleExp.extract(res = rvle_obj, file_sim = file_sim, id = id,
                                output_vars=output_vars);
    
    #compute isSim from id it is either
    # - indices of simulations to extract if file_sim is null
    # - id of simu to extract otherwise
    isSim = 1:ncol(res[[1]]);
    if (! is.null(file_sim)) {
      file_sim = rvleExp.parseSim(file_sim=file_sim, id=id);
      isSim = file_sim$id
      if (! is.null(id)) {
        if (nrow(file_sim) != length(id)){
          stop("[rvleExp] file_sim and id do not fit");
        }
        if (ncol(res[[1]]) != nrow(file_sim)){
          stop("[rvleExp] file_sim and res do not fit");
        }
      }
    } else if (! is.null(id)) {
      #id is interpreted as index
      isSim = id;
    }
    
    i = 1;
    for (var in setdiff(output_vars,"date")) {
      print(var)
      df = NULL;
      begin_date = NULL;
      for (ii in 1:length(isSim)){
        idi = isSim[ii]
        file_obsi = NULL;
        if (! is.null(file_obs) && (var %in% names(file_obs))) {
          file_obsi = subset(file_obs, id == idi);
          file_obsi = subset(file_obsi,  !is.na(file_obsi[[var]]));
          file_obsi$date = rvleExp.dateToNum(file_obsi$date);
        }
        resi = rvleExp.extract(res = res, time_ind = time_ind,
                                     date = NULL,  file_sim = file_sim, 
                                     id = idi, output_vars = c(var,"date"));
        if (! is.null(file_obsi)) {
          if(!("date" %in% names(resi))){
            stop(paste("[rvleExp.plot] Error: missing 'date' in results"));
          }
          file_obsi = subset(file_obsi, date %in% resi$date[,1])
          if (nrow(file_obsi) == 0){
            file_obsi = NULL;
          }
        }
        idistr = as.character(idi);
        if (!is.null(sim_legend)){
          idistr = sim_legend[ii];
        }
        if (! is.null(begin_date)){
          if (begin_date != resi$date[1,1]){
            stop(paste("[rvleExp] superposition sim error: ",
                       "sim dates are not equal"));
          }
        } else {
          begin_date = resi$date[1,1];
        }
        sim_vec = resi[[var]][,1];
        time_vec = 1:length(sim_vec);
        obs_vec = as.numeric(rep(NA, length(sim_vec)));
        if (! is.null(file_obsi)) {
          obs_vec[match(file_obsi$date, resi$date[,1])] = file_obsi[,var];
        }
        id_vec = rep(idistr, length(sim_vec));
        
        df = rbind(df,data.frame(id=id_vec, sim = sim_vec, time=time_vec, 
                                 obs=obs_vec, stringsAsFactors = F));
      }
      
      #build plot
      gp = ggplot2::ggplot(data=df, 
            ggplot2::aes_string(x="time", y="sim", colour="id")) + 
            ggplot2::geom_line() + ggplot2::labs(x="time", y=var);
      if (! is.null(file_obs)) {
        gp = gp + ggplot2::geom_point(ggplot2::aes_string(y="obs"), 
                                      color='black',na.rm=TRUE)
      }
      if (is.null(sim_legend)) {
        gp = gp + ggplot2::theme(legend.position="none");
      }
      gpAll[[i]] = gp;
      i = i+1;
    }
  }
  
  #return static aor dynamic plots
  if (!is.null(typeReturn) && (typeReturn == "plot_list")) {
    return (gpAll)
  }
  do.call(gridExtra::grid.arrange, gpAll);
  return (invisible(NULL)) 
}

