# Returns a string that is an R expression that builds a list of the input parameters as translated from JSON
# For example:..
#
#     str(dumper(str='hello', df=data.frame(x=c(1:2), y=c('a', 'b'))))
#
# yields the string
#
#    list(str = "hello",
#         df = structure(
#           list(x = 1:2, y = structure(
#             1:2, .Label = c("a",
#                             "b"), class = "factor"
#           )),
#           class = "data.frame",
#           row.names = c(NA,-2L)
#         ))
#' @export
dumper <- function(param1=NULL, ...) {
  dput(list(param1, ...))
}
