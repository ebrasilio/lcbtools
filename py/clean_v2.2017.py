from __future__ import print_function
from __future__ import division
# import os
# import glob
# import math
#
import numpy as np
import pandas as pd
# import matplotlib.pyplot as plt
# import fnmatch
# import copy
from LCBnet_lib import *
# from scipy import interpolate


class ManageDataLCB(object):
    """
    DESCRIPTION
        1) Read data LCB observations, make the file readable by pandas and write a new file
        2) Clean the file and Merge them into a new dataframe
            Threshold are used on every variables to filter bad values. 
            For a specific event if one variable exceed a threshold, all the other variables are removed as well.
    """

    def __init__(self, inpath, fname, staname=None, log=None):
        """
        DESCRIPTION
            Take The Path and the folder name of the raw data 
            :rtype: object

        """
        self.inpath=inpath
        self.log = log
        self.fname=fname
        self.staname = staname
        self.Header_info = 'Modulo XBee Unknown  Adress SL: Unknown\r\n'
        self.NbLineHeader = 2
        self.threshold={
                        'Pa H':{'Min':850,'Max':920},
                        'Ta C':{'Min':5,'Max':40,'gradient_2min':4},#
                        'Ua %':{'Min':0.0001,'Max':100,'gradient_2min':15},
                        'Rc mm':{'Min':0,'Max':9},
                        'Sm m/s':{'Min':0,'Max':30},
                        'Dm G':{'Min':0,'Max':360},
                        'Bat mV':{'Min':0.0001,'Max':10000},
                        'Vs V':{'Min':9,'Max':9.40}#'Vs V':{'Min':8.5,'Max':9.5}
                        }
        self.nomenclature = {"Bat":"Bat mV","Dm":"Dm G","Sm":"Sm m/s","Ta":"Ta C","Ua":"Ua %","Pa":"Pa H","Rc":"Rc mm","Vs":"Vs V"}

        self.__read(inpath,fname)
        self.__Header()
        self.__clear(log)

    def write_clean(self,OutPath,fname, log=None):
        """
        DESCRIPTION
            write the dataframe wich has been cleared
        """
        self.__clear(log)
        fname = fname.upper()
        f = open(OutPath+fname+'clear',"w")
        for line in self.content_clear:
            f.write(line)
        f.close

    def append_dataframe(self,fileobject):
        """
        DESCRIPTION
            Append the dataframes together
        USERINPUT
             A list of file path with the different files to merge
            exemple: H05XXX240 will be merged with H05XXX245
        """
        try:
            self.Dataframe = self.Dataframe.append(fileobject.Dataframe).sort_index(axis=0)
            print("Merging dataframe " + fileobject.fname)
        except:
            print('It cant merge dataframe')
    def write_dataframe(self,OutPath, fname, resample=None):
        """
        DESCRIPTION
            write the dataframe
        PARAMETERS
            resample, list of times, e.g. D,H,M
            resample="H"
        """
        df = self.Dataframe

        # if not resample:
        df.to_csv(OutPath+fname+".txt")
        print('--------------------')
        print('Writing dataframe')
        print('--------------------')

        if resample:
            for r in resample:
                dfr = df.resample(r).mean()
                print("ALALAAAAAAAAAAAAAAAAAAAAAA")
                print(df)
                dfr['Rc mm'] = df['Rc mm'].resample(r).sum()
                dfr.to_csv(OutPath +fname+"_"+r+".txt")
                print('--------------------')
                print('Writing dataframe')
                print(r+"_Averaged ")
                print("Rain is "+r+"_summed ")
                print('--------------------')

    def clean_dataframe(self, threshold = True, specific = True, reindex = True, gradient = True, merge_header=True):
        # type: (object, object, object, object, object) -> object
        """
        DESCRIPTION
            Apply filter to the dataframe
        PARAMETERS:
            threshold: if True, apply threshold to the dataframe
            specific: if True, apply specific filter made by hand
            reindex: if True, reindex to ensure the index continuity
            gradient: if True, apply filter based on the temporal variation of a variable
            merge_header:if True, Correct the header nomenclature
            :rtype: object
        """

        dataframe = self.Dataframe

        self.old_dataframe = self.Dataframe.copy()
        print('0'*80)
        dataframe = dataframe.drop_duplicates() # remove duplicated index lines

        # change nomenclature (2018)


        # Check nomenclature
        if merge_header:
            colnames = dataframe.columns
            nomenclature = self.nomenclature
            for colname in colnames:
                if colname in nomenclature.keys():
                    try:
                        dataframe[nomenclature[colname]] = dataframe[nomenclature[colname]].combine_first(dataframe[colname])
                        del dataframe[colname]
                    except KeyError:
                        dataframe = dataframe.rename(columns=nomenclature)
                        print('(error 2018 )Nao tem '+ colname)


        # Specific manual filter
        if specific:
            dataframe = self._specific_clean(dataframe)

        # Threshold
        if threshold:
            print('Apply threeshold filter')
            dataframe = self._threshold(dataframe)


        dataframe = dataframe.convert_objects(convert_numeric=True) # convert the data frame into numeric if not put nan
        dataframe = dataframe.dropna(axis = 1, how ='all') # remove null colomns (e.g. Unnamed)

        # Reindex
        if reindex:
            dataframe = self._reindex(dataframe)

        # Gradient thresholds
        if gradient:
            dataframe = self._grad_threshold(dataframe,'Ta C')
            dataframe = self._grad_threshold(dataframe,'Ua %')

        self.Dataframe = dataframe
        print('0'*80)

    def plot_comparison_clean(self,vars = None, just_clean = False, subplot = False, outpath='/home/thomas/',staname = None, From=None, To=None):
        """
        DESCRIPTION
            Plot time serie before and after the data cleaned
        PARAMETERS
            vars: name of the variables to be plotted
            just_clean: True, plot only the cleaned data
            subplot: if True, make subplots
            outpath: if not None, save the figure to the indicated path
            staname: name of the station
            From: starting date
            To: final date
        """
        old_dataframe = self.old_dataframe
        dataframe = self.Dataframe

        if From:
            old_dataframe = old_dataframe[From:To]
            dataframe = dataframe[From:To]

        if vars == None:
            vars = dataframe.columns

        if subplot:
            f, axarr = plt.subplots(len(vars), sharex=True,figsize=(1920/96, 1080/96), dpi=96)
            if len(vars) ==1:
                axarr = [axarr]
            for ax,var in zip(axarr, vars):
                if not just_clean:
                    ax.plot(old_dataframe.index, old_dataframe[var], color='red')
                    ax.plot(dataframe.index, dataframe[var], color='blue')
                elif just_clean:
                    ax.plot(dataframe.index, dataframe[var], color='blue')

            plt.savefig(outpath+"_"+var[0:2]+"_"+staname, dpi=96)
        else:
            for var in vars:
                try:
                    plt.figure(figsize=(1920/96, 1080/96), dpi=96)
                    if not just_clean:
                        plt.plot(old_dataframe.index, old_dataframe[var], color='red')
                        plt.plot(dataframe.index, dataframe[var], color='blue')
                    elif just_clean:
                        plt.plot(dataframe.index, dataframe[var], color='blue')
                    plt.savefig(outpath+"_"+var[0:2]+"_"+staname, dpi=96)
                except ValueError:
                    print ('Could not plot surely a problem with the variable: ', str(var), " in the uncleaned dataframe")

    def __read(self, inpath, fname):
        """
        DESCRIPTION
            Read the raw file
            check if the data can be read in a dataframe
            if not, it si clean with the function clear()
        """
        with open(inpath + fname) as f:
            content = f.readlines()
        self.content = content

        print("======================================================================================")
        print('OPEN THE FILE: '+self.fname)
        try:
            self.Dataframe = pd.read_csv(self.inpath+self.fname, sep=',', header=1, index_col=0, parse_dates=True)
            print('The file is readable - A dataframe has been created')
        except:
            print('The file is Dirty! - I cant create a dataframe - Clean before and import again')

    def __clear(self, log=None):
        """
        DESCRIPTION
            This methods delete all the line which does not fit some criteria
            At the end it permits to create a data frame readeable by pandas
        """
        LineToDel=[]
        content_clear=self.content

        # Check if the file is empty
        if not content_clear:
            print("-> The File is empty")
            content_clear.insert(0,self.Header_info)
            content_clear.insert(1,self.__Header(type=1))
            print('New title',content_clear[0])

        # Check title information
        if content_clear[0][0:11] != self.Header_info[0:11]:
            print("-> Rewrite title info")
            content_clear.insert(0,self.Header_info)
            print('New title',content_clear[0])

        # Check the header
        if content_clear[1][0:4] != self.Header_var[0:4]:
            print("-> No Header -> Rewrite Header columns")
            content_clear.insert(1,self.Header_var)





        # Deleting unreadeable lines
        for idx,line in enumerate(content_clear[self.NbLineHeader::]):

            # if len(line.split(',')) != self.__nbcol() or line[0:1].isdigit() == False or line[-2:]!='\r\n' or len(line.split(',')[0])!=16:
            if len(line.split(',')) != self.__nbcol() or line[0:1].isdigit() == False or len(line.split(',')[0]) != 16:
                # print('-> Deleting the line ',idx+self.NbLineHeader," :",line)
                LineToDel.append(self.NbLineHeader+idx)
        if log and len(LineToDel)>0:
            thefile = open(self.log + "log"+ self.fname , 'w')

        for i in sorted(LineToDel, reverse=True):
            if len(LineToDel)>0:
                pass
                #print>> thefile, content_clear[i]
            del content_clear[i]

        self.content_clear = content_clear  # This should be at the end, why it is not


        # Check the header
        if len(self.content_clear) >2:
            if  len(content_clear[1].split(',')) > len(content_clear[2].split(',')):
                print("-> Less data columns than header _> Rewrite header")
                del content_clear[1]
                content_clear.insert(1,self.__Header(type=2))
            if len(content_clear[1].split(',')) < len(content_clear[2].split(',')):
                print("-> More data columns than header -> Rewrite header")
                del content_clear[1]
                content_clear.insert(1,self.__Header(type=1))

    def __Header(self,type=None):
        """
        DESCRIPTION
            Return the header in function of the type of the file
        NOTE
            The file header changed several times, especially at the begining when the 
            file pattern was not yet defined by Nilson
        """
        if type ==1:
            var='T_St,Bat mV,Dm G,Sm m/s,Ta C,Tint C,Ua %,Pa H,Rc mm,Vs V\r\n'
            return var

        if type ==2:
            var='T_St,Bat mV,Dm G,Sm m/s,Ta C,Ua %,Pa H,Rc mm,Vs V\r\n'
            return var

        if self.fname[0] =='C' or self.fname[0] =='c':
            self.Header_var='T_St,Bat mV,Dm G,Sm m/s,Ta C,Tint C,Ua %,Pa H,Rc mm\r\n'

        if self.fname[0] == 'H' or self.fname[0] == 'h' :
            self.Header_var='T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm,,\r\n'

    def __nbcol(self):
        """
        DESCRIPTION
            Return the number of column in the header of the file opened
        """
        nbcol = len(self.Header_var.split(','))
        return nbcol

    def _reindex(self,dataframe):
        """
        Reindex with a 2min created index
        
        NOTE
            The nan are not represented in the graphic 
            compared to the missing value wich are linearly interpolated on a plot
            So after reindexing the time serie plot appears differently and this is normal
            
            If during a period their is some sparse data, for example 4 or 5 data by day.
            without nan the serie will look like their is data but it is not the case.
        """
        Initime = dataframe.index[0]
        Endtime = dataframe.index[-1]
        Initime=pd.to_datetime(Initime)# be sure that the dateset is a Timestamp
        Endtime=pd.to_datetime(Endtime)

        # remove all nan to be sure that the reindex does not keep bad values
        dataframe = dataframe.dropna(axis = 0, how ='all')

        dataframe = dataframe.drop_duplicates() # I do not know which this one does not work properly
        dataframe = dataframe.groupby(dataframe.index).first()# take the first index encontered. A mthod to drop duuplicate
        idx=pd.date_range(Initime,Endtime,freq='2min')
        dataframe = dataframe.reindex(index=idx)
        return dataframe

    def _threshold(self,dataframe):
        """
        DESCRIPTION
            Remove all the data above the threesholds
        RETURN 
            New clean dataframe
        NOTE
            pour les stations C05, C08 et C09
            il y a des problemes de PA et Ua qui enleve des bonne valeurs de TA et Sm 
            donc pour ces 3 stations, seulement la valeur de la variables est drop est non pas 
            toute la ligne
            
            Pour le cross specific il faut que la variable qui permet
             de filtrer ne soit pas deja filtrer elle meme
        """
        print ("-> Threshold")
        threshold = self.threshold
        newdataframe = dataframe.copy()
        staname = self.staname

        for var in newdataframe.columns:
            print (var)
            if (staname == "C05" and var == "Rc mm")\
            or (staname == "C05" and var == "Ua %"):
                newdataframe = self._specific_threshold(newdataframe, var, var2 = 'Pa H')
                newdataframe = self._specific_threshold(newdataframe, var, var2 = 'Ua %')
                newdataframe = self._specific_threshold(newdataframe, var, var2 = 'Rc mm')

            if (staname == "C04" and var == "Rc mm"):
                newdataframe = self._specific_threshold(newdataframe, var, var2 = 'Ua %')

            if (staname == "C15" and var == "Ta C"):
                threshold = {"Vs V":{'Min':9.1,'Max':9.40}}
                newdataframe = self._specific_threshold(newdataframe, var, var2 = 'Vs V', threshold=threshold)

            if (staname == "C05" and var == "Pa H")\
                   or (staname == "C05" and var == "Ua %")\
                   or (staname == "C05" and var == "Rc mm")\
                   or (staname == "C05" and var == "Vs V")\
                   or (staname == "C08" and var == "Ua %")\
                   or (staname == "C08" and var == "Pa H")\
                   or (staname == "C09" and var == "Ua %")\
                   or (staname == "C04" and var == "Ua %")\
                   or (staname == "C04" and var == "Pa H")\
                   or (staname == "C18" and var == "Pa H")\
                   or (staname == "C18" and var == "Ua %")\
                   or (staname == "C18" and var == "Rc mm")\
                   or (staname == "C09" and var == "Pa H"):
                print ("specific threshold")

                newdataframe = self._specific_threshold(newdataframe, var)
            else:
                try:
                    index = newdataframe[(newdataframe[var]<threshold[var]['Min']) | (newdataframe[var]>threshold[var]['Max']) ].index
                    newdataframe = newdataframe.drop(index)
                except KeyError:
                    print('no threshold for '+var)




        return newdataframe

    def _specific_threshold(self, df, var, var2 = None, threshold=None):
        """
        DESCRIPTION
            do a specific threshold which will only remove a variable and
            not the entire column
        INPUT
            var2 = True, use var1 to clean var2
            Threshold= None, use the by default threeshold
        """
        print ("-> Specific threshold")

        if not threshold:
            threshold = self.threshold

        if not var2:
            df[var][(df[var]<threshold[var]['Min']) | (df[var]>threshold[var]['Max']) ] = np.nan
        else:
            print ("cross variable filtering")
            df[var][(df[var2]<threshold[var2]['Min']) | (df[var2]>threshold[var2]['Max']) ] = np.nan
        return df

    def _grad_threshold(self,df, var):
        """
        DESCRIPTION 
            Remove value exceding a threshold gradient
        """
        print ("-> Gradient")
        threshold = self.threshold
        gradient = df[var].diff().abs()
        df[var][gradient > threshold[var]['gradient_2min']] = np.nan
        return df

    def _var_blocked(self, df, var, var2= None):
        # type: (object, object, object) -> object
        """
        DESCRIPTION
            Check if a variable is blocked in time
            - Resample by hours and check if the variable is exactly the same on the next hour
            which is impossible to be exactly the same with the same degree of accuracy
        
        INPUT
            var: string, variable to check the stationnarity
            var2: other variable to remove based on var (for example sm/ms when Dm G is stationnary)
        """
        print (df.columns)
        print (var)
        print (df.loc[:,var])
        print (df.shape)



        print ("removing blocked wind")
        # df['diff_var'] = df.loc[:,var].resample('1H',how='sum').diff().asfreq('2Min', method='pad') # problem version numpy Emilia

        a = df.loc[:, var].resample('1H', how='sum')

        b = a.astype(np.float32)
        diff = b[:-1] - b[1:]
        diff = pd.DataFrame(diff, index=a.index[:-1])

        df['diff_var'] = diff.asfreq('2Min', method='pad')


        df[var][df['diff_var'] == 0 ] = np.nan
        df[var2][df['diff_var'] == 0 ] = np.nan

        return df
        False

    def _specific_clean(self,df):
        """
        DESCRIPTION
            Do some specific cleaning on: 
            - specific stations
            - specific period
        NOTE
            - Correct rain accumulation into rain intensity
            - Correct time delay
        """
        print (df)

        if self.staname == "C06":
            try:
                print (df[:"2014-09-04 13:36:00"]['Rc mm'])
                df[:"2014-09-04 13:36:00"]['Rc mm'] = df[:"2014-09-04 13:36:00"]['Rc mm'].diff(periods=1).abs()
                print (df[:"2014-09-04 13:36:00"]['Rc mm'].diff(periods=1).abs())
                print (df[:"2014-09-04 13:36:00"]['Rc mm'])
            except IndexError:
                print ('Not in index')

        if self.staname == "C05":
            try:
                df[:"2014-10-02 17:10:00"]['Rc mm'] = df[:"2014-10-02 17:10:00"]['Rc mm'].diff(periods=1).abs()
            except IndexError:
                print ('Not in index')

        if self.staname == "C04":
            try:
                df[:"2014-10-02 17:44:00"]['Rc mm'] = df[:"2014-10-02 17:44:00"]['Rc mm'].diff(periods=1).abs()
            except IndexError:
                print ('Not in index')

        if self.staname == "C08":
            try:
                df[:"2014-09-04 12:32:00"]['Rc mm'] = df[:"2014-09-04 12:32:00"]['Rc mm'].diff(periods=1).abs()
                df["2014-09-17 14:02:00":"2014-10-09 16:10:00"]['Rc mm']  = np.nan
                df["2014-12-22 00:00:00":"2014-12-24 00:00:00"]['Rc mm']  = np.nan
            except IndexError:
                print ('Not in index')

        if self.staname == "C15":
            try:
                df[ "2015-09-12 00:00:00":"2015-09-14 00:00:00"]['Rc mm']  = np.nan
            except IndexError:
                print ('Not in index')

        if self.staname == "C09":
            try:
                df[:"2014-09-04 10:36:00"]['Rc mm'] = df[:"2014-09-04 10:36:00"]['Rc mm'].diff(periods=1).abs()
                df["2014-09-17 13:32:00":"2014-10-09 13:56:00"]['Rc mm'] = np.nan
                df["2014-09-02 20:52:00":"2014-09-05 10:32:00"]['Rc mm'] = np.nan
            except IndexError:
                print ('Not in index')

        if self.staname == "C18":
            try:
                index = df[:"2015-05-08 00:00:00"].index - pd.DateOffset(hours = 3)
                index = index.append(df["2015-05-08 00:00:00":].index)
                df.index = index
            except IndexError:
                print ('Not in index')

        if self.staname == "C18":
            try:
                df = self._var_blocked(df, 'Dm G', 'Sm m/s')
                df = self._var_blocked(df, 'Sm m/s', 'Dm G')
            except IndexError:
                print ('Not in index')

        if self.staname == "C17":
            try:
                df = self._var_blocked(df, 'Dm G', 'Sm m/s')
                df = self._var_blocked(df, 'Sm m/s', 'Dm G')
            except IndexError:
                print ('Not in index')

        if self.staname == "C19":
            try:
                df = self._var_blocked(df, 'Dm G', 'Sm m/s')
                df = self._var_blocked(df, 'Sm m/s', 'Dm G')
            except IndexError:
                print ('Not in index')

        if self.staname == "C08":
            try:
                df = self._var_blocked(df, 'Dm G', 'Sm m/s')
                df = self._var_blocked(df, 'Sm m/s', 'Dm G')
            except IndexError:
                print ('Not in index')


        if self.staname == "C04":
            try:
                df = self._var_blocked(df, 'Dm G', 'Sm m/s')
                df = self._var_blocked(df, 'Sm m/s', 'Dm G')
            except IndexError:
                print ('Not in index')

        if self.staname == "C06":
            try:
                df= df.drop(df["2014-09-19 00:00:00":"2014-09-21 00:00:00"].index)
            except IndexError:
                print ('Not in index')


        if self.staname == "C05":
            try:
                df['Ua %'][:"2014-09-17 14:52:00"]= np.nan
            except IndexError:
                print ('Not in index')

        return df


if __name__=='__main__':
#===========================================================================
# 1) Clean the files to be readble by Pandas - padronizado
#===========================================================================
    # inpath='/data1/DATA/LCB/EXT/STORAGE/data/C/raw'
    # OutPath='/data1/DATA/LCB/EXT/STORAGE/data/C/Clean/'
    # logpath = '/data1/DATA/LCB/EXT/STORAGE/data/C/log/'
    #
    # Files=glob.glob(inpath+"/*") # Find all the files
    # print (Files)
    # for i in Files:
    #  # create instance of managedatalcb object
    #  managedata = ManageDataLCB(os.path.dirname(i)+"/", os.path.basename(i), log=logpath)
    #  print("Writing file " + OutPath + os.path.basename(i))
    #  managedata.write_clean(OutPath, os.path.basename(i)) # rewrite the files in a clean way



    InPath='/data1/DATA/LCB/EXT/STORAGE/data/C/Clean'
    OutPath='/data1/DATA/LCB/EXT/STORAGE/data/C/merge/'
    OutPath_r ='/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample/'

# station C21 deu problema
#    stations = [ 'C20', 'C21', 'C23', 'C24', 'C25', 'C26']
    stations = ['C03','C04','C05','C07','C08','C13','C15','C17','C19','C20','C21','C23','C24','C25','C26']

    # stations = ['C26']  #stations =['C23','C24','C25','C26']
    #stations =['C04']

    for sta in stations:
        print(sta)
        matches = []
        datamerge = None

        # find all the cleared files
        for root, dirnames, filenames in os.walk(InPath):
            for filename in fnmatch.filter(filenames, sta+'*.TXTclear'):
                matches.append(os.path.join(root, filename))

        if matches == []:
            print("EMILIA THE PATH IS WRONG")
        print(matches)
        for i in matches:
            print(i)
            print(os.path.dirname(i)+"/", os.path.basename(i))
            managedata = ManageDataLCB(os.path.dirname(i)+"/", os.path.basename(i), sta) # create an instance of managedatalcb object
            try:
                datamerge.append_dataframe(managedata) # group all the managedatalcb instance
            except:
                datamerge=managedata
        datamerge.clean_dataframe() # filter and reconstruct the data
        datamerge.write_dataframe(OutPath, sta+'clear_merge') # write the dataframe
        datamerge.write_dataframe(OutPath_r, sta + 'clear_merge', resample=['H', 'D'])  # write the dataframe
