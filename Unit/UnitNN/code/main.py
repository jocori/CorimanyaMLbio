#DAN:
#I am glad you did something for this assignment, but the dataset is not really one for which
#sustained investigation with NNs is appropriate. The mushroom data are more suited for CARTs
#or some basic method. This is reflected in some of your results, like the fact that some of
#your models are massively overfitted. 
#
#One thing that is impressive is you undertook to do everything in python.
#
#There are other shortcomings besides what I've listed above, but the above shortcoming
#kinda prefigures all the others. 
#
#Grade: S-

print("Starting Neural Network Assignment Analysis...")

# Run Day 1 and 2
print("\nRunning Days 1 and 2: First Neural Networks...")
exec(open("Days01and02.py").read())

# Run Day 3 and 4
print("\nRunning Days 3 and 4: Convolutional Neural Network and CNN Optimizer Comparison...")
exec(open("Day02_AlternativeNNs.py").read())


# Day 5: Transfer Learning
print("\nRunning Day 5: Transfer Learning...")
exec(open("Day05_TransferLearning.py").read())

print("\nAnalysis Complete.")
