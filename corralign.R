#Copyright (C) 2019  Mert Ketenci

#This program is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' Form correlation based sorted data frames
#'
#' Especially developed for ggplot's parcoord,
#' the algorithm sorts the attributes of a given data frame with respect to their correlation,
#' forming a meaningful pattern from something that may seem meaningless.
#' For a detailed description and visual example please refer to: https://github.com/ketencimert/corralign
#' It is important to indicate that the output data frame is normalized, meaning that the values are between 0 and 1.
#' In the next versions we are planning to add a new feature that will enable keeping values as they are
#' depending on the choice of the user.
#'
#' @param data_frame,begining_axis are data frame and character respectively.
#'
#' @return an object of \code{data frame} class
#'
#' @importFrom stats cor
#'
#' @examples
#'
#' data_frame=data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
#' begining_axis="X1"
#' sorted_frame=corralign(data_frame,begining_axis)
#'
#' @export

corralign = function(data_frame,begining_axis){
  #Begining axis is a character

  rest=colnames(data_frame)[!colnames(data_frame)==begining_axis]
  begining_order=c(begining_axis,rest)
  data_frame=data_frame[,begining_order]

  for (i in 1:ncol(data_frame)){
    data_frame[,i]=(data_frame[,i]-min(data_frame[,i]))/(max(data_frame[,i])-min(data_frame[,i]))
  }


  csubset_w=as.data.frame(cor(data_frame)) #correlation matrix
  csubset_w=csubset_w[!(row.names(csubset_w) %in% begining_axis), ]#Remove the begining axis
  order=c() #An empty array to store the order
  order=c(begining_axis,order)

  for (i in 1:nrow(csubset_w)){

    sequence=rownames(csubset_w)[which(csubset_w==min(csubset_w[,
                                                                grep(order[i],
                                                                     colnames(csubset_w))]),
                                       arr.ind=TRUE)[1]]
    order=c(order,sequence)
    row.names.remove <- rownames(csubset_w)[which(csubset_w==min(csubset_w[,
                                                                           grep(order[i],
                                                                                colnames(csubset_w))]),
                                                  arr.ind=TRUE)[1]]

    csubset_w=csubset_w[!(row.names(csubset_w) %in% row.names.remove), ]

  }

  data_frame=data_frame[,order]


  return(data_frame)

}
