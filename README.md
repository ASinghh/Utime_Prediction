# Utime_Prediction
BUILD A MODEL TO PREDICT THE UTIME(total transcoding time for transcoding) OF A LARGE YOUTUBE VIDEO MATA-DATASET. THE DATASET HAD MANY CONTINUOUS PREDICTORS SO I TRIED VARIOUS REGRESSION METHODS. 
I TRIED MULTIPLE REGRESSION, LASSO AND RIDGE. LASSO OUT PERFORMED ALL OTHERS WITH AN RMSE OF 8.7




Picked up the data from the UC Irvine library(https://archive.ics.uci.edu/ml/datasets/Online+Video+Characteristics+and+Transcoding+Time+Dataset)
The presented dataset is composed of two tsv files named 'youtube_videos.tsv' 
and 'transcoding_mesurment.tsv'. The first contains 10 columns of fundamental 
video characteristics for 1.6 million youtube videos; It contains YouTube video id, 
duration, bitrate(total in Kbits), bitrate(video bitrate in Kbits), 
height(in pixle), width(in pixles), framrate, estimated framerate, codec, 
category, and direct video link. This dataset can be used to gain insight 
in characteristics of consumer videos found on UGC(Youtube). 

The second file of our dataset contains 20 columns(see column names for names) 
which include input and output video characteristics along with their transcoding 
time and memory resource requirements while transcoding videos to diffrent but 
valid formats. The second dataset was collected based on experiments on an Intel 
i7-3720QM CPU through randomly picking two rows from the first dataset and using 
these as input and output parameters of a video transcoding application, ffmpeg 4 . 
In section 6 we will use the second dataset to build a transcoding time prediction 
model and show the significance of our datasets.


Attribute Information:

id = Youtube videp id 
duration = duration of video 
bitrate bitrate(video) = video bitrate 
height = height of video in pixles 
width = width of video in pixles 
frame rate = actual video frame rate 
frame rate(est.) = estimated video frame rate 
codec = coding standard used for the video 
category = YouTube video category 
url = direct link to video (has expiration date) 
i = number of i frames in the video 
p = number of p frames in the video 
b = number of b frames in the video 
frames = number of frames in video 
i_size = total size in byte of i videos 
p_size = total size in byte of p videos 
b_size = total size in byte of b videos 
size = total size of video 
o_codec = output codec used for transcoding 
o_bitrate = output bitrate used for transcoding 
o_framerate = output framerate used for transcoding 
o_width = output width in pixel used for transcoding 
o_height = output height used in pixel for transcoding 
umem = total codec allocated memory for transcoding 
utime = total transcoding time for transcoding



