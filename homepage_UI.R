#Homepage UI, for 'Intro' tab, 'Methods' tab and 'Cancer types' tab
homepage <- tabItem(tabName = "home", 
                    tabsetPanel(
                      tabPanel("Intro", 
                               HTML("<h3 align = 'center'><b> Welcome to the ABPEPserver! </h3></b><br>
	                                   <div>
  	                                   Cancer cells experience tryptophan shortage as a consequence of interferon-gamma (IFN) signaling pathway. 
  	                                   It has been recently demonstrated that such tryptophan shortage leads to tryptophan to phenylalanine codon reassignment (W>F) 
  	                                   resulting into aberrant peptide product. <br>
  
                                       ABPEPserver offers the visualization of a large-scale proteomics analysis of multiple human cancer types in which aberrant peptides are detected. 
                                       In this analysis, tryptophan to phenylalanine (W>F) codon reassignment were found to be a vastly abundant phenomenon in multiple cancer types. 
                                       Furthermore, these W>F mis-incorporations, called W>F substitutants, were found to be enriched in tumors as compared with adjacent normal tissues, 
                                       and often their appearance was associated with T-cell and oncogenic signaling activities.<br>
  	       
                              	       Proteomic cancer data from multiple cancer types, hosted by <a href='https://proteomic.datacommons.cancer.gov/pdc/'>PDC commons</a> database, 
                              	       have been used in this analysis. An in depth description of how the analysis was conducted is shown in the '<b>Methods</b>' tab.
                              	       The '<b>Cancer types</b>' tab shows cancer types analyzed and includes information about each proteomic dataset.<br>
                              	       This server will enable ease and fast access to aberrant peptides, which are potent immunotherapeutic targets and hence of high value.<br>
                              	       <p>Results are available in: </p>
                              	       <ul><li>Barplots depicting cumulative number of W-substitutants detected in tumour and adjacent normal tissue samples.</li>
                              	       <li>Violin plots depicting the number of substitutions detected in low (intensity < 0) and high (intensity >0) tumor and adjacent normal tissue samples.</li>
                              	       <li>Scatter contour plots depicting for every gene the number of W>F substitutions when the gene is higher expressed (intensity > 0) on X-axis (High Class) 
                              	       and when the gene is lower expressed (intensity < 0) on Y-axis (Low Class).</li>
                              	       <li>Excel file outputs with the list of aberrant peptides.</li></ul></div><br>
              
                                       <div style='display: flex; align-items: center;'>
                                       <div style='padding-right: 10px;'><h4><b>Background information</h4></b><br>
                                       <p>T-cells infiltrating tumour microenvironment secrete IFN-gamma (IFNγ) which activates Indoleamine 2,3-dioxygenase 1 (IDO1) 
                                       enzyme expression in cancer cells. IDO1 stimulates pathways that subvert T-cell immunity while also breaking down tryptophan in its process, 
                                       leading to a tryptophan deficiency.<sup>1</sup> When a healthy cell is deficient in tryptophan, ribosomes will stop translation at the codon for 
                                       tryptophan. Therefore, protein synthesis will stop. Nevertheless, in Melanoma cells protein synthesis continues despite interferon gamma (IFNγ)-mediated 
                                       tryptophan depletion. They skip the firstnucleotide of the codon for tryptophan. Consequently, the whole reading frame jumps from that point on leading 
                                       to the synthesis of aberrant proteins as a result of ribosomal frameshifting at tryptophan codons. <sup>2</sup><br>
                                       
                                       In follow up studies, it was shown that IFNγ-mediated tryptophan shortage also impacts protein quality by generating specific mis-incorporations of 
                                       phenylalanine at tryptophan codons. This type of amino acid deprivation-induced codon reassignments are called ‘substitutants’ to distinguish them 
                                       from somatic genetic substitutions. Therefore, the absence of the essential amino acid tryptophan causes ribosomes to induce errors of two-types, i.e, 
                                       ribosomal frame-shifting and amino-acid substitution. </p>
                                     </div>
                                     <img src='subs.png' style='border: 2px solid;' width='350x'/></div><br>
                                     <h4><b>Notes</h4></b>
                                      <p>1.   Champagne J, Pataskar A, Blommaert N, et al. Oncogene-dependent sloppiness in mRNA translation 
                                              [published online ahead of print, 2021 Sep 21]. Mol Cell. 2021;S1097-2765(21)00736-X. doi:10.1016/j.molcel.2021.09.002.<br>
                                         2.    Bartok, O., Pataskar, A., Nagel, R. et al. Anti-tumour immunity induces aberrant peptide presentation in melanoma. 
                                               Nature 590, 332–337 (2021). https://doi.org/10.1038/s41586-020-03054-1.
                                      </p>"
                                    )),
            tabPanel("Methods", 
            HTML("<div><br>
                  <div style='display: flex; align-items: center;'>
                  <div>
                  <h4><b>An important toolkit that has been be used in this analysis is Philosopher<sup>1</sup>, 
                  a versatile toolkit for shotgun proteomics data analysis. This tool uses mass spectrometry data to rapidly do peptide quantification analysis, 
                  search for PTMs, and many more relevant proteomics analyses.<br><br>
                  Furthermore, this analysis can be divided into several steps:
                  <ul>
        	          <li> Creating a custom protein sequence database</li>
        	          <li> Obtaining required data and formatting them properly for the Philosopher pipeline</li>
            		    <li> Adjustment of Philosopher parameter file for desired analysis</li>
            		    <li> Launch proteomics data analysis with Philosopher pipeline toolkit</li>
            		    <li> Processing results</li>
          	      </ul><br></b></h4>
      	          
                  <h4><b> Creating a custom protein sequence database</b></h4>
                  The human proteome was downloaded from UNIPROT56 (release-
                  2011_01) , and all instances of tryptophan amino-acids in the
                  proteome were changed to all other amino acids except Lysine and Arginine, in order to
                  avoid creation of tryptic cleavage site when generating tryptic peptides in the scan. <br>
                  
                  <br><h4><b> Obtaining required data and formatting them properly for the Philosopher pipeline</b></h4>
                  To automatically download datasets and format them properly, a custom pipeline was written to:
                  <ul>
        	          <li> Download proteomic dataset from <a href='https://proteomic.datacommons.cancer.gov/pdc/'>PDC</a> database in
                         MZML file format </li>
        	          <li> Download the corresponding sample data and create an annotation file containing mappings <br>
        	               between the TMT channels and the sample labels for each of the whole proteome plexes </li>
            		    <li> Create a folder for each of the whole proteome plexes and their annotation file </li>
            		    <li> Create folder named <b>bin</b> for software tools to be used (MSFragger, Philosopher and TMTIntegrator)</li>
            		    <li> Adjust the parameter file (explained in the following section) </li>
            		    <li> Create a folder named <b>params</b> for the parameters file </li>
            		    <li> Create a folder named <b>database</b> for the protein sequence FASTA file </li>
        	       </ul><br></div>
        	       <img src='Philosopher_workflow.png' width='350px style='max-width: 100%; height: auto;'/></div>
                
                 <h4><b>Adjustment of Philosopher parameter file for desired analysis </b></h4>
                  By using the automated pipeline mode, all the necessary steps will automatically run. For this, the parameter file was used.<br>

                  MSFragger<sup>2</sup> was used for peptide detection with the following parameters; Precursor mass lower: -20 ppm, Precursor mass upper: 20 ppm, 
                  precursor mass tolerance: 20ppm, calibrate mass: TRUE, Deisotoping: True, mass offset: FALSE, isotope error: STANDARD, digestion: Strictly tryptic 
                  (Max. missed cleavage: 2), Variable modifications (For iTRAQ datasets): 15.99490 M 3, 42.01060 [^ 1, 229.162932 n^ 1, 229.162932 S 1, 
                  Variable modifications (For TMT datasets): 15.99490 M 3, 42.01060 [^ 1, 144.1021 n^ 1, 144.1021 S 1, Min Length: 7, Max Length: 50, 
                  digest mass range: 500:5000 Daltons, Max Charge: 2, remove precursor range: -1.5, 1.5, topN peaks: 300, minimum peaks: 15, precursor range: 1:6, 
                  add Cysteine: 57.021464, add Lysine (for ITRAQ datasets): 144.1021, add Lysine (for TMT datasets): 229.162932,among other basic parameters. <br>
                  
                  PeptideProphet<sup>3</sup> was then used for Peptide Validation with following parameters (accmass: TRUE, decoyprobs: TRUE, expectScore: TRUE, Glycosylation: FALSE, 
                  ICAT: FALSE, masswidth: 5 , minimum probability after first pass of a peptide: 0.9, minimum number of NTT in a peptide: 2, among other parameters.<br>
                  
                  Next, isobaric quantification was undertaken separately for TMT and iTRAQ datasets with following parameters (bestPSM: TRUE, level: 2, minProb 0.7, 
                  ion purity cut-off: 0.5, tolerance: 20 ppm, among other parameters. Thereafter, FDR filtering was implemented to retain only confident peptides 
                  with following parameters (FDR < 0.01, peptideProbability: 0.7, among other parameters. <br>
      
                  Thereafter, TMT-integrator<sup>1</sup> was used to integrate Isobaric Quantification with following parameters (retention time normalization: FALSE, minimum peptide
                  probability on top of FDR filtering (TMT datasets): 0.9, minimum peptide probability on top of FDR filitering (for iTRAQ 32 dataset): 0.5, among other parameters). <br>
            "),
              br(), a(href="TMT11.txt", "Download TMT11 parameter file", download=NA, target="_blank"),
              br(), a(href="TMT10.txt", "Download TMT10 parameter file", download=NA, target="_blank"),
            HTML("<br><br>
                  <h4><b> Launch proteomics data analysis with Philosopher pipeline toolkit </b></h4>
                  Philosopher pipeline was used to detect all peptides in mass-spectrometry datasets (MZML files), including the substitutant peptides.
                  To start the pipeline, the next Philosopher pipeline command is used:<br><br>
      
                  <div id='content'>
                  <p style='padding: 0.5em'>$ bin/philosopher pipeline --config params/[parameter file] [list of proteome plex directories]</p>
                  </div><br>
      
                  
                  <h4><b> Processing results</h4></b>
                  Substitutant peptides were fetched from the reports of TMT Integrator, and any detected peptide intensity score for a sample normalized
                  to the reference channel above 0 (log-scale) was considered as a positive peptide for that sample using a R-script.
                  R was used to plot density plots as well as Barplots for number of peptide detections. Next, protein expression profiles for each cancer type was downloaded in
                  already analysed format from PDC database. PERL scripts were designed to count number of substitutants when a gene is lowly expressed (intensity <0) or
                  highly expressed (intensity >0). 
                 </div><br>
                  <h4><b>Notes</h4></b>
                  <p>1.   da Veiga Leprevost F, Haynes SE, Avtonomov DM, Chang HY, Shanmugam AK, Mellacheruvu D, Kong AT, Nesvizhskii AI. Philosopher: a versatile toolkit 
                          for shotgun proteomics data analysis. Nat Methods. 2020 Sep;17(9):869-870. doi: 10.1038/s41592-020-0912-y.<br>
                     2.   Kong, A. T., Leprevost, F. V., Avtonomov, D. M., Mellacheruvu, D., & Nesvizhskii, A. I. (2017). MSFragger: ultrafast and comprehensive peptide identification 
                          in mass spectrometry–based proteomics. Nature Methods, 14(5), 513-520.<br>
                     3.   Keller A, Nesvizhskii A, Kolker E, Aebersold R: Empirical statistical model to estimate the accuracy of peptide identifications made by MS/MS and database search. 
                          Analytical Chemistry. 2002, 74: 5383-5392. 10.1021/ac025747h.
                  </p><br><br><br>"
                 )),
            tabPanel("Cancer types", 
                     HTML(
              "<div class='parent'>
                <div class='tab_cancers'>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"CCRCC\")'>Clear Cell Renal Carcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"GBM\")'>Glioblastoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"HNSCC\")'>Head and Neck Squamous Cell Carcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"liver\")'>Hepatocellular Carcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"LSCC\")'>Lung Squamous Cell Carcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"OV\")'>Ovarian Serous Cystadenocarcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"PDA\")'>Pancreatic Ductal Adenocarcinoma</button></h4>
                  <h4><button class='tablinks_cancers' onclick='openCancer(event, \"UCEC\")'>Uterine Corpus Endometrial Carcinoma</button></h4>
                </div>
                
                <div id='CCRCC' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Clear Cell Renal Carcinoma</h3><br>

                  <button type='button' class='collapsible'>Study details</button>
                  
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000127</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/31675502/'>Integrated Proteogenomic Characterization of Clear Cell Renal Cell Carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                  <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT10</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>23</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>25</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>110</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>84</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='ccrcc.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                
                <div id='GBM' class='tabcontent_cancers'>
                <h3>Glioblastoma</h3><br>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000204</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/33577785/'>Proteogenomic and metabolomic characterization of human glioblastoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>24</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>100</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>10</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='gbm.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='HNSCC' class='tabcontent_cancers'>
                <h3>Head and Neck Squamous Cell Carcinoma</h3><br>
                
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000221</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/33417831/'>Proteogenomic insights into the biology and treatment of 
                      HPV-negative head and neck squamous cell carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>20</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>25</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>105</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>53</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='hnscc.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='liver' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Hepatocellular Carcinoma</h3><br>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000198</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/31585088/'>Integrated Proteogenomic Characterization of HBV-Related Hepatocellular Carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>33</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>48</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>165</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>165</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='liver.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='LSCC' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Lung Squamous Cell Carcinoma</h3><br>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000234</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/34358469/'>A proteogenomic portrait of lung squamous cell carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>22</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>25</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>110</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>101</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='lscc.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='OV' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Ovarian Serous Cystadenocarcinoma</h3><br>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000110</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/33086064/'>Integrated Proteomic and Glycoproteomic Characterization of Human High-Grade Serous Ovarian Carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT10</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>13</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>24</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>84</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>22</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='ov.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='PDA' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Pancreatic Ductal Adenocarcinoma</h3><br>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000270</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/34534465/'>Proteogenomic characterization of pancreatic ductal adenocarcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT11</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>25</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>25</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>137</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>74</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='pda.png' style='max-width: 100%;'/>
                  </div>
                </div>
                
                <div id='UCEC' class='tabcontent_cancers'>
                <div style='display: flex;'>
                  <div style='width: 100%;'>
                  <h3>Uterine Corpus Endometrial Carcinoma</h3><br>
                  <button type='button' class='collapsible'>Study details</button>
                  <div class='col_content'>
                    <table>
                    <tr>
                      <td id='cancer_table'><b>PDC Study Identifier</b></td>
                      <td id='cancer_table'>PDC000125</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Study</b></td>
                      <td id='cancer_table'><a href='https://pubmed.ncbi.nlm.nih.gov/32059776/'>Proteogenomic Characterization of Endometrial Carcinoma</a></td> 
                    </tr>
                  </table>
                  </div>

                  <button type='button' class='collapsible'>LC-MS/MS details</button>
                  <div class='col_content'>
                    <table style='width: 100%;'>
                    <tr>
                      <td id='cancer_table'><b>Quantitation strategy</b></td>
                      <td id='cancer_table'>Isobaric label quantitation</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b>Isobaric Labeling reagent</b></td>
                      <td id='cancer_table'>TMT10</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># proteome plexes</b></td>
                      <td id='cancer_table'>17</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># fractions per proteome plex</b></td>
                      <td id='cancer_table'>24</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># tumour samples</b></td>
                      <td id='cancer_table'>100</td> 
                    </tr>
                    <tr>
                      <td id='cancer_table'><b># adjacent normal sample</b></td>
                      <td id='cancer_table'>49</td> 
                    </tr>
                  </table>
                  </div>

                  </div>
                  <img src='ucec.png' style='max-width: 100%;'/>
                  </div>
              </div></div> "
            )))
)










