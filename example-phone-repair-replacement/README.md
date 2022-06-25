# Example: Phone - Repair & Replacement

phone.R contains the full code

diagram.jpg is a graphical illustration of the Markov Process

- In any given day, assuming a phone has had i faults (for i=0,1,2,...,n-1) the probability of having another fault is p, independently of all previous days, and the new fault is to be repaired with probability qi+1 within the same day.
- If a phone is not repaired, it is replaced instead.
- If a phone has its n'th fault it is immediately replaced.
- The expected cost to ABC to repair the i'th fault of a phone is $(q*100*2i).
- Replacing a phone costs ABC $R.
- ABC's products fall into one of three classes:
    1. Low quality - n=3; R=410; p=5*10^-4 for 25% of the production line.
    2. Medium quality - n=4; R=850; p=2*10^-4, for 52% of the production line.
    3. High quality - n=4; R=950; p=10^-4, for 23% of production line.
- Replacement phones initially have zero faults and are subject to the same warranty.
- Replacement phones always have the same quality class as the originals.
- The probability of experiencing more than one fault within a single day is sufficiently small and is to be ignored.

Motivation:
- ABC is interested in understanding the expected daily cost of the warranty scheme for every phone and the optimal replacement strategy. In particular, ABC can invest in its maintenance department to increase q, which increases the proportion of faults that can be repaired but also increases the average repair cost.

You are to compute the expected daily costs for each of the three quality classes. First, report these costs for q=0.8 then produce plots showing the expected total daily costs for all values of q from 0 to 1 and each of the three quality classes. Lastly, recommend the best value of q, up to an error of 1%, in two cases:
- Different values may be used for different quality classes to optimize the total cost of each class;
- The same value is to be used in all quality classes to optimize the total cost;
and report on the total expected daily costs in each case.
