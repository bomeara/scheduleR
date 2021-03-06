% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{host_import}
\alias{host_import}
\title{Gathers hosts input data and formats}
\usage{
host_import(
  data,
  slot_minutes = 15,
  input_minutes = 30,
  extract_last_name = TRUE
)
}
\arguments{
\item{data}{Input CSV file}

\item{slot_minutes}{How long to make each slot}

\item{input_minutes}{Resolution of the input slots}

\item{extract_last_name}{If TRUE, extract last name; if FALSE, assume only last name given}
}
\value{
a list with availability array and a room vector (where names are last names)
}
\description{
This takes a CSV with columns for Name, Room, and Times (separated by commas and spaces). This sort of file could come from a Google Sheet, for example, where hosts have filled in information about times in a form with checkboxes. If there are times like 8:30 am, 9 am, etc. and slot_minutes are 15 minutes, it will assume that if someone selects 8:30 they can make both 8:30 and 8:45 slots. It assumes last names are unique and uses the humaniformat package to extract them. If you want more precise slots, you can have slot_minutes less than input_minutes: they said this, say, 30 minute slot was available, which means that both 15 minute slots within could be free.
}
