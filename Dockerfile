FROM rocker/tidyverse:3.5.3

RUN sudo apt-get update                                                                               
RUN sudo apt-get install -y libjpeg-dev                                        
RUN sudo apt-get install -y libnetcdf-dev                                      
RUN sudo apt-get install -y libudunits2-dev                                    
RUN sudo apt-get install -y libgdal-dev                                        
                                                                               
# Install disperser                                                            
RUN R -e "devtools::install_github('lhenneman/disperseR')"
RUN R -e "devtools::install_github( 'lhenneman/SplitR')"
