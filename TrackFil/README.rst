About TrackFil
============

The TrackFil software provides an algorithm to automatically track filaments on Sun images.

TrackFil was initially developped in the framework of the HELIO Virtual Observatory European project (http://www.helio-vo.eu/),
to populate its Heliophysics Feature Catalogue (HFC).
TrackFil data results on the Meudon Spectroheliograph observations are available in the HFC web portal at: http://hfc.helio-vo.eu.

The software algorithm is described in details in: http://dx.doi.org/10.1007/s11207-012-9985-9.

Note that TrackFil software assumes that the solar filaments to be tracked have been previously detected.
For instance, the HFC filaments are detected using the Fuller et al., 2005 alogorithm (see 10.1007/s11207-005-8364-1).

Content
======

The TrackFil package contains the TrackFil source code, as well as scripts to
install and configure the software.

It stores the following items:

::

     bin/           binary files (e.g., idl runtime file to launch trackfil as a script).
     
     config/      configuration files providing the metadata and algorithm input parameters.
     
     data/         can be used to store input data files.
     
     hfc/           contains the wrapper for the HFC
     
     lib/            contains external libraries required to run trackfil
     
     logs/         can be used to store log file
     
     products/ can be used to store trackfil data products
     
     scripts/     scripts to set up and run trackfil.
     
     src/           code source files (written in IDL).
     
     tmp/          can be used to store temporary files
     
     tools/        extra tools (e.g., program to train trackfil).

Installation
=========

System requirements
------------------------------

Trackfil requires IDL 7.0 or higher.

The main SolarSoft (SSW) package shall be also installed and callable on your system.
(Visit http://www.lmsal.com/solarsoft/ for more details).

Trackfil can only by run using the (t)csh shell.

How to get Trackfil
------------------------------

You can download the TrackFil package from Github, entering:

::

    git clone https://github.com/HELIO-HFC/TrackFil.git

This will create a local "TrackFil" directory containing the TrackFil software.

*In the next sections, all of the commands are executed assuming you are in the TrackFil/ directory.*

How to set up Trackfil
------------------------------

Before set up and run Trackfil, be sure that SSW can be loaded in IDL using the "sswidl" command.

Then, enter:

::

    source scripts/setup_trackfil.csh

If everything goes right, it should create a "trackfil.sav" file in the bin/ subdirectory.

How to run Trackfil
------------------------------

Open a IDL interpreter session, then enter:

::

    restore,'bin/trackfil.sav',/VERBOSE

This will loaded all of the Trackfil compiled routines.

The Trackfil main program is called "trackfil". Enter "trackfil" in the interpreter should return something like:

::

    % TRACKFIL: Usage:
    Results = trackfil(fil_data, config_file=config_file, /SILENT)


About the TrackFil algorithm
======================

Context
------------

TrackFil performs the tracking of filaments detected over successive Sun disk observations.
It assumes that all of the filaments have been previoulsy detected, and that possible fake detections have been isolated and removed.

It also requires to provide, for each filament detected, the following parameters:
    - The filament pruned skeleton
    - The filament contour (encoded using a chain code mechanism)

Principles
--------------

The TrackFil algorithm actually combines two tracking algorithms in one:

    - A close position algorithm, used to track the smaller filaments and to filter too farest features.

    - A curve matching algorithm, which performs a morphological analysis of filaments,
     to identify the same feature between two observations.

Both algorithms are computed on a Modified Carrington (MC) map, where filaments' centroids are quasi-static relative to the time and space.

The close position algorithm
----------------------------------------

The *close position algorithm* (CP) compares the MC position of the filaments on N successive observations.
If filaments stay in the same area, they are assumed to be the same feature as seen on the successive images.
Depending on the input parameters provided, the filaments are then compared using the curve matching algorithm in order to confirm or not the first assumption.

The curve matching algorithm
------------------------------------------

The *curve matching algorithm* (CM) use a least-square method to compare the shape of two filament skeletons. It returns a probability that the two skeletons have the same shape. This probability serves as a basis to define the confidence of tracking.

About the *trackfil* procedure
=======================

This *trackfil* IDL procedure in the src/ directory is the main TrackFil program.

Input arguments
-----------------------

Two main input arguments are required to run "trackfil":

    - fil_data, which is a IDL structure containing the detected filament parameters.

    - config_file, which is the path to the TrackFil configuration file. This file stores the input parameters to be used by the alogorithms, as well as several additional metadata to identify the dataset.


*fil_data* shall be a IDL structure with the following tags:

    - fil_id                index of the filament, (shall be unique).
    - track_id           index of the filament tracking (equal at fil_id at the beginning of the process)
                                Same filaments will have the same track_id number between observations.
    - date_obs      date of observation (string of ISO8601 format)
    - jdint              date of observation in Julian days (Integer part)
    - jdfrac             date of observation in Julian days (Fraction part)
    - ske_length_deg        Length of the filament skeleton in degrees
    - ske_cc            chain code of the filament skeleton
    - ske_cc_x_pix     X coordinate in count of the first skeleton pixel
    - ske_cc_y_pix     Y coordinate in count of the first skeleton pixel
    - cdelt1                   Pixel resolution over X axis (in arcsec/pix)
    - cdelt2                   Pixel resolution over Y axis (in arcsec/pix)
    - center_x              X coordinate in pixels of the Sun center
    - center_y              Y coordinate in pixels of the Sun center
    - r_sun                    Solar radius in pixels
    - phenom                Index of the phenomena detection
    - ref_id                    Index of filaments for a solar rotation to the next

Configuration file
..............................

The TrackFil configuration file is a simple text file with *key = value* pairs - one pair per row - providing the value of the Trackfil algorithm parameters to be used.

Note that the quality of the tracking depends on the algorithm values given in the configuration file. The optimal values can vary significantly from a dataset to another. The TrackFil team does not provide yet any specific tool to perform the algorithm performance assessment. This task shall be independently perform by the users from their own datasets.

The configuration file shall provide the following keywords:

    - LMIN              Minimal filament skeleton length to run the CM algorithm
    - DS                Length of a skeleton segment in the CM algorithm (used to perform Curvilinear interpolation)
    - RMAX          Maximal distance in degrees between two filaments to be compared for tracking
    - THRESHOLD         Probability threshold used in the tracking confidence computation
    - A0                             a0 weight factor used in the tracking confidence computation
    - THETA0                 theta0 weight factor used in the tracking confidence computation
    - D0                      d0 weight factor used in the tracking confidence computation
    - DT0                   dt0 weight factor used in the tracking confidence computation
    - HG_LONG_LIM   absolute Heliographic longitude in degrees, over which the tracking is not performed anymore.

