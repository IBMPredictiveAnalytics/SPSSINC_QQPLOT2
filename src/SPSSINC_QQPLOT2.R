#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/


#__author__ = "SPSS, JKP"
#__version__ = "1.0.0"

# history
# 23-jan-2009  original version
# 28-may-2014 convert to pure R package



helptext="SPSSINC QQPLOT2 VARIABLES=one or two variable-names
[BY=variable]
[OPTIONS TITLE=text]

Split files and weight are not honored by this command.

VARIABLES specifies one or two variable names.  If there are two, their
distributions are compared.  The y variable comes first.
If there is one, a grouping variable must be specified, and the 
groups defined by that variable will be compared.

BY specifies a grouping variable, which should have exactly two values.  If
there are more, the first two will be used.

TITLE can be specified as a quoted string to replace the default title.

PROGRAMFILE is no longer supported.

SPSSINC QQPLOT2 /HELP prints this information and does nothing else.

Example:
SPSSINC QQPLOT2 VARIABLES=mpg BY=type.
"

doqqplot = function(variables, by=NULL, thetitle=NULL, programfile=NULL) {
    #run R qqplot function

    setuplocalization("SPSSINC_QQPLOT2")
    
    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("QQPLOT2")
    warningsprocname = gtxt("QQPLOT2: Warnings")
    omsid="SPSSINCQQPLOT2"
    warns = Warn(procname=warningsprocname,omsid=omsid)
    
    if (!is.null(programfile)) {
        warns$warn(gtxt("The PROGRAMFILE keyword is no longer supported"), 
        dostop=TRUE)
    }

    allvars = c(variables, by)
    dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode="labels")
    dict = spssdictionary.GetDictionaryFromSPSS(allvars)
    if (!is.null(by)) {  # one-variable plus group plot
        if (length(variables) != 1) {
            warns$warn(gtxt("Exactly one variable must be specified if there is a grouping variable"),
            dostop=TRUE)
        }
        dta[[2]] = factor(dta[[2]])   # need to get rid of any irrelevant labels and NAs
        numlevels = nlevels(dta[[length(dta)]])
        if (numlevels <= 1) {
            warns$warn(gtxt("The BY variable is constant or is not a factor"),
            dostop=TRUE)
        }
        if (numlevels > 2) {
            # first two levels only
            dta[[2]] = factor(dta[[2]], levels=levels(dta[[2]])[1:2])
        }
        ylabel = paste(ifelse(dict[["varLabel", 1]] == "", variables[[1]], dict[["varLabel", 1]]), 
            gtxt(" (first group)"))
        xlabel = paste(ifelse(dict[["varLabel", 1]] == "", variables[[1]], dict[["varLabel", 1]]), 
            gtxt(" (second group)"))
        subtitle = paste(gtxt("Groups: "), ifelse(dict[["varLabel", 2]] == "", by, dict[["varLabel",2]]),
            ': ')
        subtitle = paste(subtitle, paste(levels(dta[[2]])[1:2], collapse=", "), collapse=" ")
        dta = split(dta[[1]], dta[[2]])
        if (is.null(thetitle)) {
            title = gtxt("Two Group Q-Q Plot")
        }
    }
    else {  # two-variable plot
        if (length(variables) != 2) {
            warns$warn(gtxt("Exactly two variables must be specified if there is no grouping variable"),
                dostop=TRUE)
        }
        ylabel = ifelse(dict[["varLabel", 1]] == "", variables[[1]], dict[["varLabel", 1]])
        xlabel = ifelse(dict[["varLabel", 2]] == "", variables[[2]], dict[["varLabel", 2]]) 
        if (is.null(thetitle)) {
            thetitle = gtxt("Q-Q Plot")
        }
        subtitle=""
    }
    
    rr = range(as.matrix(dta), na.rm=TRUE, finite=TRUE)

    qqplot(dta[[2]], dta[[1]], ylab=ylabel, xlab=xlabel, 
      col="blue", pch=16, xlim=rr, ylim=rr,
      main=thetitle)
    title(sub=subtitle)
    abline(a=0, b=1)
}


Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment
    
    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.
        
        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 
        
        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any
        
        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                },
                error = function(e) {
                    FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                                               gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)
                
                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                                                spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}

gtxt <- function(...) {
    return(gettext(...,domain="SPSSINC_QQPLOT2"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="SPSSINC_QQPLOT2"))
}
    
    
Run = function(args) {
    #Execute the SPSSINC QQPLOT2 command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("VARIABLES", subc="", ktype="existingvarlist", var="variables",islist=TRUE),
        spsspkg.Template("BY", subc="",  ktype="existingvarlist", var="by", islist=FALSE),
        spsspkg.Template("TITLE", subc="OPTIONS", ktype="literal", var="thetitle"),
        spsspkg.Template("MISSING", subc="OPTIONS",ktype="str", var="missing"),
        spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),

        spsspkg.Template("HELP", subc="", ktype="bool")
    ))

# A HELP subcommand overrides all else
if ("HELP" %in% attr(args,"names")) {
    helper(cmdname)
}
else {
    res <- spsspkg.processcmd(oobj, args, "doqqplot")
    }
}

helper = function(cmdname) {
  # find the html help file and display in the default browser
  # cmdname may have blanks that need to be converted to _ to match the file

  fn = gsub(" ", "_", cmdname, fixed=TRUE)
  thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
  if (is.null(thefile)) {
    print("Help file not found")
  } else {
    browseURL(paste("file://", thefile, sep=""))
  }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}