function LL=drew_LL_cur(x)p1=x(1);p2=x(2);G=x(3);LL=- 2*log(p2/(exp(1/G) + 1)^10 - (p1 + p2 - 1)/(exp(-1/G) + 1)^10 + p1/(exp(-1/G) + 1)^10) - log(p1/((exp(1/G) + 1)*(exp(-1/G) + 1)^9) - (p1 + p2 - 1)/(exp(1/G) + 1)^10 + p2/((exp(1/G) + 1)^9*(exp(-1/G) + 1))) - 3*log(p1/((exp(1/G) + 1)^8*(exp(-1/G) + 1)^2) - (p1 + p2 - 1)/((exp(1/G) + 1)^3*(exp(-1/G) + 1)^7) + p2/((exp(1/G) + 1)^2*(exp(-1/G) + 1)^8)) - log(p1/((exp(1/G) + 1)^3*(exp(-1/G) + 1)^7) - (p1 + p2 - 1)/((exp(1/G) + 1)^4*(exp(-1/G) + 1)^6) + p2/((exp(1/G) + 1)^7*(exp(-1/G) + 1)^3)) - 2*log(p1/((exp(1/G) + 1)^5*(exp(-1/G) + 1)^5) - (p1 + p2 - 1)/((exp(1/G) + 1)^4*(exp(-1/G) + 1)^6) + p2/((exp(1/G) + 1)^5*(exp(-1/G) + 1)^5)) - 2*log(p1/(exp(1/G) + 1)^10 + p2/(exp(-1/G) + 1)^10 - (p1 + p2 - 1)/((exp(1/G) + 1)*(exp(-1/G) + 1)^9)) - log(p2/(exp(1/G) + 1)^10 + p1/(exp(-1/G) + 1)^10 - (p1 + p2 - 1)/((exp(1/G) + 1)^9*(exp(-1/G) + 1)));if ~isreal(LL) | isnan(LL)LL=1e6;end