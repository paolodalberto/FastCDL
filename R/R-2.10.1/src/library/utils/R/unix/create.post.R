create.post <- function(instructions = "\\n",
                        description = "post",
                        subject = "",
                        ccaddress = Sys.getenv("USER"),
                        method = getOption("mailer"),
                        address ="the relevant mailing list",
                        file = "R.post")
{
    methods <- c("mailx", "gnudoit", "none", "ess")
    method <-
	if(is.null(method)) "none"
	else methods[pmatch(method, methods)]

    body <- paste(instructions,
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
		  paste(names(R.version),R.version, sep=" = ",collapse="\\n "),
                  if (nzchar(Sys.getenv("R_GUI_APP_VERSION")))
                      paste("\\n\\nGUI:\\n R-GUI ",Sys.getenv("R_GUI_APP_VERSION"),
                            " (",Sys.getenv("R_GUI_APP_REVISION"),")",sep='')
                  else
                      ""
                  ,
                  "\\n\\n",
                  "Locale:\\n",
                  Sys.getlocale(),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")

    if(method == "gnudoit") {
	cmd <- paste("gnudoit -q '",
		     "(mail nil \"", address, "\")",
		     "(insert \"", body, "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'",
		     sep="")
	system(cmd)
    } else if(method=="none") {
        disclaimer <-
            paste("# Your mailer is set to \"none\" (default on Windows),\n",
                  "# hence we cannot send the, ", description, " directly from R.\n",
                  "# Please copy the ", description, " (after finishing it) to\n",
                  "# your favorite email program and send it to\n#\n",
                  "#       ", address, "\n#\n",
                  "######################################################\n",
                  "\n\n", sep = "")

        cat(disclaimer, file=file)
	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=TRUE)
        cat("The", description, "is being opened for you to edit.\n")
	system(paste(getOption("editor"), file))
        cat("The unsent", description, "can be found in file", file, "\n")
    } else if(method == "mailx") {
        if(missing(subject)) stop("'subject' missing")

	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file, append=FALSE)
        cat("The", description, "is being opened for you to edit.\n")
	system(paste(getOption("editor"), file))

        if(is.character(ccaddress) && nzchar(ccaddress)) {
            cmdargs <- paste("-s '", subject, "' -c", ccaddress,
                             address, "<", file, "2>/dev/null")
        }
        else
            cmdargs <- paste("-s '", subject, "'", address, "<",
                             file, "2>/dev/null")

        status <- 1L
        answer <- readline(paste("Email the ", description, " now? (yes/no) ",
                                 sep = ""))
        answer <- grep("yes", answer, ignore.case=TRUE)
        if(length(answer)) {
            cat("Sending email ...\n")
            status <- system(paste("mailx", cmdargs))
            if(status)
                status <- system(paste("Mail", cmdargs))
            if(status)
                status <- system(paste("/usr/ucb/mail", cmdargs))

            if(status == 0L) unlink(file)
            else{
                cat("Sending email failed!\n")
                cat("The unsent", description, "can be found in file",
                    file, "\n")
            }
        } else
            cat("The unsent", description, "can be found in file",
                file, "\n")
    }
    else if(method == "ess") {
	body <- gsub("\\\\n", "\n", body)
	cat(body)
    }
}
