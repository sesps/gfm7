{
  TFile *f = new TFile("output.root","recreate");
  TTree *t = new TTree();
  t->ReadFile("output.mca","x/F:y/F:e/F:q/F:t/F");
  t->Write("");
  if(!((TCanvas *) gROOT->FindObject("cFit"))) {
    TCanvas * cFit=new TCanvas("cFit","cFit",0,0,675,615);
    if(!(cFit->GetShowEventStatus()))cFit->ToggleEventStatus();
    if(!(cFit->GetShowToolBar()))cFit->ToggleToolBar();
  }
  cFit->Divide(2,2);
  cFit->cd(1);
  t->Draw("x:y","","col");
    cFit->cd(2);
  t->Draw("x:t","","col");
  cFit->cd(3);
  t->Draw("x:e","","col");
  cFit->cd(4);
  t->Draw("t:e","","col");
}
