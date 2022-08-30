//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoMain.h"
#include "InPlaceEditorsDemoFrameManager.h"
#include "InPlaceEditorsDemoValue.h"
#include "InPlaceEditorsDemoComboBoxes.h"
#include "InPlaceEditorsDemoImage.h"
#include "InPlaceEditorsDemoText.h"
#include "InPlaceEditorsDemoMultiLineText.h"
#include "InPlaceEditorsDemoCheckBoxes.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGraphics"
#pragma link "dxBar"
#pragma link "dxRibbon"
#pragma link "dxRibbonForm"
#pragma link "cxEdit"
#pragma link "cxShellComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxBlobEdit"
#pragma link "cxRadioGroup"
#pragma link "cxCheckBox"
#pragma link "cxLabel"
#pragma link "cxTextEdit"
#pragma link "cxColorComboBox"
#pragma link "cxImageComboBox"
#pragma link "cxFontNameComboBox"
#pragma link "cxCheckComboBox"
#pragma link "cxSpinEdit"
#pragma link "cxProgressBar"
#pragma link "cxTrackBar"
#pragma link "cxTimeEdit"
#pragma link "cxCalendar"
#pragma link "cxCheckGroup"
#pragma link "cxCalc"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxButtonEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxMaskEdit"
#pragma link "cxImage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxMRUEdit"
#pragma link "cxDateUtils"
#pragma link "dxmdaset"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"
TfrmMain *frmMain;

int DemoTabsCount = 6;
int RibbonTabsCount = 9;

AnsiString TabCaptions[9] = {"Value Editors", "Multi-line Text Editors", "Text Editors",
  "Image Editors", "Combo Boxes", "Check Boxes & Groups", "Styles", "Ribbon options", "Help"};

char
  *SToolBarDescription = "Edit values using bar editors and see the results below",
  *SDemoDescription = "This demo illustrates the use of thirty different in-place editors from the ExpressEditors"
    " Library. Select Help | About for more information.",
  *SRibbonOptionsDescription = "Use these options to customize Ribbon control appearance settings and toggle the Ribbon UI on/off.",
  *SEditorStyles = "Choose an in-place editor to be displayed by a TcxBarEdit item and apply different styles to the item''s caption and editor.",
  *SImageEditDescription = "Load images in bar editors and see the results below",
  *SCheckBoxesDescription = SToolBarDescription,
  *SMultiLineTextEdits = SToolBarDescription,
  *SFileFindError = "File %s not found";
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TdxCustomRibbonForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  mdContacts->LoadFromBinaryFile("..\\..\\Data\\Contacts.dat");  
  CreateFrames();

  ((TWinControl*)(dxBarDockControl1))->Align = alTop;
  lblDemoDescription->Caption = SDemoDescription;
  Panel1->Align = alClient;
  Panel2->Align = alClient;
  mdContacts->Open();

  BarManager->BeginUpdate();
  try
  {
    InitializeComboBoxes();
    InitializeCheckBoxes();
    InitializeImageEditors();
    InitializeMultilineTextEditors();
    InitializeTextEditors();
    InitializeValueEditors();
    InitializeEditorStyles();
    InitializeRibbon();

    InitializeFrames();

    BarManager->Style = bmsOffice11;

    UpdateActionsImages();

    BarManager->Bars->Items[1]->Visible = true;
    SelectNonRibbonTab(1);
  }
  __finally
  {
    BarManager->EndUpdate(true);
  };
}

void __fastcall TfrmMain::btnEditorStylesClick(TObject *Sender)
{
  SelectNonRibbonTab(((TComponent*)(Sender))->Tag);
}

void __fastcall TfrmMain::HideAllFrames()
{
  for(int I = 0; I < EditorDemoFrameManager()->FramesCount; I++)
    ((TForm*)(EditorDemoFrameManager()->Frames[I]))->Hide();
};

void __fastcall TfrmMain::ShowAllDemoToolbars(bool AVisible)
{
  BarManager->BeginUpdate();
  try
  {
    for (int I = 0; I < BarManager->Bars->Count; I++)
      if (BarManager->Bars->Items[I]->Tag != 0)
        BarManager->Bars->Items[I]->Visible = AVisible;
  }
  __finally
  {
    BarManager->EndUpdate(true);
  }
};

void __fastcall TfrmMain::SelectNonRibbonTab(int ATabIndex)
{
  ShowAllDemoToolbars(false);
  int APos = 0;
  BarManager->BeginUpdate();
  try
  {
    for (int I = 0; I < (BarManager->Bars->Count); I++)
      if  (BarManager->Bars->Items[I]->Tag == ATabIndex)
      {
        BarManager->Bars->Items[I]->Move(dxBarDockControl1, APos, 0);
        BarManager->Bars->Items[I]->Visible = True;
        APos++;
      }
  }
  __finally
  {
    BarManager->EndUpdate(true);
  };
  SelectTab(ATabIndex);
};

void __fastcall TfrmMain::SelectTab(int ATabIndex)
{
  HideAllFrames();
  if (ATabIndex <= EditorDemoFrameManager()->FramesCount)
    EditorDemoFrameManager()->Frames[ATabIndex - 1]->Show();
};

void __fastcall TfrmMain::DockAllDemoToolBars(TdxBarDockControl* ADockControl)
{
  for (int I = 1; I <= DemoTabsCount; I++)
    BarManager->Bars->Items[I]->DockControl = dxBarDockControl1;
}

void __fastcall TfrmMain::RibbonTabChanged(TdxCustomRibbon* Sender)
{
  SelectTab(Sender->ActiveTab->Tag);
};

void __fastcall TfrmMain::InitializeStylesCombo(TcxBarEditItem* ABarEditItem, TStrings* AStyles)
{
  ((TcxComboBoxProperties*)(ABarEditItem->Properties))->Items->AddStrings(AStyles);
  ABarEditItem->EditValue = ((TcxComboBoxProperties*)(ABarEditItem->Properties))->Items->Strings[0];
};

void __fastcall TfrmMain::ReadTextFile(TcxBarEditItem* AEditor, AnsiString AFileName)
{
  TStringList* AList;
  AList = new TStringList;
  try
  {
    if (FileExists(AFileName))
    {
      AList->LoadFromFile(AFileName);
	  AEditor->EditValue = AList->Text;
    }
	else
	{
	  TVarRec args[1] = {AFileName};
	  AnsiString S = SFileFindError;
	  ShowMessage(Format(S, args, 0));
		}
  }
  __finally
  {
	delete AList;
  };
};

void __fastcall TfrmMain::CreateFrames()
{
  EditorDemoFrameManager()->AddFrame(new TfrmValueEditors(this), bfcValueEditors);
  EditorDemoFrameManager()->AddFrame(new TfrmMultiLineTextEditors(this), bfcMultilineTextEditors);
  EditorDemoFrameManager()->AddFrame(new TfrmTextEditors(this), bfcTextEditors);
  EditorDemoFrameManager()->AddFrame(new TfrmImageEditors(this), bfcImageEditors);
  EditorDemoFrameManager()->AddFrame(new TfrmComboBoxes(this), bfcComboBoxes);
  EditorDemoFrameManager()->AddFrame(new TfrmCheckBoxes(this), bfcCheckBoxes);
  EditorDemoFrameManager()->AddFrame(new TEditorDemoBaseFrame(this), bfcStyles);
  EditorDemoFrameManager()->AddFrame(new TEditorDemoBaseFrame(this), bfcRibbonOptions);
}

void __fastcall TfrmMain::UpdateCheckBoxesView()
{
  TfrmCheckBoxes* AFrame = dynamic_cast<TfrmCheckBoxes*>(EditorDemoFrameManager()->Frames[bfcCheckBoxes]);
  AFrame->SetParameters(rgSelectColor->EditValue, chgSelectColor->EditValue, (int)chbMonochrome->EditValue);
};

void __fastcall TfrmMain::UpdateComboBoxesView()
{
  Graphics::TBitmap* AImage = new Graphics::TBitmap;
  try
  {
    ilSmall->GetImage(imcImages->EditValue, AImage);
    TfrmComboBoxes* AFrame = dynamic_cast<TfrmComboBoxes*>(EditorDemoFrameManager()->Frames[bfcComboBoxes]);
    AFrame->SetParameters((TColor)((int)(clcFontColor->EditValue)), fncPathFontName->EditValue,
      scbSelectPath->EditValue, cbFontSize->EditValue, AImage, mdContacts->FieldByName("FullName")->AsString);
  }
  __finally
  {
    delete AImage;
  };
};

void __fastcall TfrmMain::UpdateImageEditorsView()
{
  TStringStream* AStream;
  
  if (VarIsNull(edtImage->EditValue))
    AStream = NULL;
  else
  {
	AStream = new TStringStream(dxVariantToString(edtImage->EditValue));
  }
  try
  {
    TfrmImageEditors* AFrame = dynamic_cast<TfrmImageEditors*>(EditorDemoFrameManager()->Frames[bfcImageEditors]);
    AFrame->SetParameters(AStream);
  }
  __finally
  {
    delete AStream;
  };
};

void __fastcall TfrmMain::UpdateMultilineTextEditorsView()
{
  TStringStream* AStream = new TStringStream(dxVariantToString(memMemo->EditValue));
  try
  {
    TfrmMultiLineTextEditors* AFrame = dynamic_cast<TfrmMultiLineTextEditors*>(EditorDemoFrameManager()->Frames[bfcMultilineTextEditors]);
    AFrame->SetParameters(0, AStream);
  }
  __finally
  {
    delete AStream;
  };
};

AnsiString _fastcall TfrmMain::GetDateValue()
{
  if (VarIsNull(edtDate->EditValue))
    return ("");
  else
    return (edtDate->EditValue);
}

void __fastcall TfrmMain::UpdateValueEditorsView()
{
  TfrmValueEditors* AFrame = dynamic_cast<TfrmValueEditors*>(EditorDemoFrameManager()->Frames[bfcValueEditors]);
  AFrame->SetParameters(prbFontSize->EditValue, GetDateValue(), edtTime->EditValue, edtMoney->EditValue);
};

void __fastcall TfrmMain::UpdateTextEditView()
{
  TfrmTextEditors* AFrame = dynamic_cast<TfrmTextEditors*>(EditorDemoFrameManager()->Frames[bfcTextEditors]);
  AFrame->SetParameters(edtCompanyName->EditValue, edtSite->EditValue, mePhoneNum->EditValue);
};

void __fastcall TfrmMain::cbSelectEditTypeChange(TObject *Sender)
{
  int AIndex = ((TcxComboBoxProperties*)(cbSelectEditType->Properties))->Items->IndexOf(cbSelectEditType->EditValue);
  TMetaClass* APropertiesClass = (GetRegisteredEditProperties()->Items[AIndex]);
  TcxBarEditItem* AItem;

  for (int AIndex = 0; AIndex < BarManager->ItemCount; AIndex++)
  {
    AItem = dynamic_cast<TcxBarEditItem *>(BarManager->Items[AIndex]);
    if ((AItem != 0)&&(AItem->PropertiesClass == APropertiesClass)&&((AItem->Category > 0)&&(AItem->Category <= DemoTabsCount)))
    {
      edtPreviewItem->EditValue = Null;
      edtPreviewItem->PropertiesClass = APropertiesClass;
      edtPreviewItem->Properties->Assign(AItem->Properties);
      edtPreviewItem->EditValue = AItem->EditValue;
      edtPreviewItem->Caption = cbSelectEditType->EditValue;
    };
  };
};

void __fastcall TfrmMain::cbStyleChange(TObject *Sender)
{
  SetEditorStyle("Style", cbStyle);
}

void __fastcall TfrmMain::FormDestroy(TObject *Sender)
{
  mdContacts->Close();
  delete EditorDemoFrameManager();
};

void __fastcall TfrmMain::UpdateActionsImages()
{
  btnDockingHelp->ImageIndex = 18;
  btnBarsHelp->ImageIndex = 18;
  btnDXOnTheWeb->ImageIndex = 20;
  btnMyDX->ImageIndex = 20;
  btnDownloads->ImageIndex = 20;
  btnProducts->ImageIndex = 20;
  btnSupport->ImageIndex = 21;
  btnRateDemo->ImageIndex = 22;
  btnAbout->LargeImageIndex = 19;
  btnAbout->ImageIndex = 19;
};

void __fastcall TfrmMain::cbRibbonFontChange(TObject *Sender)
{
  SynchronizeRibbonFontEditors();
}

void __fastcall TfrmMain::ccbAssignedRibbonFontsChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    AnsiString AValue;
    AValue = ccbAssignedRibbonFonts->EditValue;
    TdxRibbonAssignedFonts AFonts = FRibbon->Fonts->AssignedFonts;
    for (int I = 1; I <= ((TcxCheckComboBoxProperties*)(ccbAssignedRibbonFonts->Properties))->Items->Count; I++)
      if (AValue[I] == '1')
        AFonts << (TdxRibbonAssignedFont)(I - 1);
      else
        AFonts >> (TdxRibbonAssignedFont)(I - 1);
    FRibbon->Fonts->AssignedFonts = AFonts;
    SynchronizeRibbonFontEditors();
  };
};

void __fastcall TfrmMain::btnRibbonStyleClick(TObject *Sender)
{
  if (btnRibbonStyle->Down)
  {
    BarManager->BeginUpdate();
    try
    {
      tlbMainMenu->Visible = false;
      DockAllDemoToolBars(0);
      FRibbon->BarManager = BarManager;
      ShowAllDemoToolbars(true);
      FRibbon->Visible = true;
      FRibbon->SupportNonClientDrawing = true;
      tlbRibbonOptions->Visible = true;
      tlbHelp->Visible = true;
      FRibbon->Tabs->Items[0]->Active = true;
      lblDemoDescription->Align = alBottom;
      ((TcxCheckGroupProperties*)(chgSelectColor->Properties))->Columns = 3;
      ((TcxRadioGroupProperties*)(rgSelectColor->Properties))->Columns = 3;
      SelectTab(1);
    }
    __finally
    {
      BarManager->EndUpdate(true);
    };
  }
  else
  {
    BarManager->BeginUpdate();
    try
    {
      tlbRibbonOptions->Visible = false;
      tlbHelp->Visible = false;
      FRibbon->SupportNonClientDrawing = false;
      FRibbon->Visible = false;
      FRibbon->BarManager = 0;
      btnValueEdits->Down = true;
      ShowAllDemoToolbars(false);
      tlbMainMenu->Visible = true;
      BarManager->Bars->Items[1]->Visible = true;
      ((TcxCheckGroupProperties*)(chgSelectColor->Properties))->Columns = 1;
      ((TcxRadioGroupProperties*)(rgSelectColor->Properties))->Columns = 1;
      SelectNonRibbonTab(1);
    }
    __finally
    {
      BarManager->EndUpdate(true);
    };
    lblDemoDescription->Align = alTop;
  };
};

void __fastcall TfrmMain::seFontSizeChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    SynchronizeValueEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateValueEditorsView();
  };
};

void __fastcall TfrmMain::edtCompanyNameChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    if (ActiveBarControl() != NULL)
      ActiveBarControl()->HideAll();
    SynchronizeTextEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateTextEditView();
  };
};

void __fastcall TfrmMain::edtMoneyChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    SynchronizeCalcValueEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateValueEditorsView();
  };
};

void __fastcall TfrmMain::InitializeComboBoxes()
{
  scbSelectPath->EditValue = GetCurrentDir();
  clcFontColor->EditValue = clWindow;
  EditorDemoFrameManager()->Frames[bfcComboBoxes]->SetDescription(SToolBarDescription);
};

void __fastcall TfrmMain::InitializeCheckBoxes()
{
  chbMonochrome->EditValue = false;
  EditorDemoFrameManager()->Frames[bfcCheckBoxes]->SetDescription(SCheckBoxesDescription);
};

void __fastcall TfrmMain::InitializeImageEditors()
{
  SynchronizeImageEditors(edtImage->EditValue);
  UpdateImageEditorsView();
  EditorDemoFrameManager()->Frames[bfcImageEditors]->SetDescription(SImageEditDescription);
};

void __fastcall TfrmMain::InitializeMultilineTextEditors()
{
  ReadTextFile(reRich, "EditorCategories.rtf");
  SynchronizeMultilineTextEditors(memPopup->Lines->Text);
  UpdateMultilineTextEditorsView();
  EditorDemoFrameManager()->Frames[bfcMultilineTextEditors]->SetDescription(SMultiLineTextEdits);
};

void __fastcall TfrmMain::InitializeTextEditors()
{
  edtCompanyName->EditValue = "Developer Express Inc.";
  EditorDemoFrameManager()->Frames[bfcTextEditors]->SetDescription(SToolBarDescription);
}

void __fastcall TfrmMain::InitializeValueEditors()
{
  edtTime->EditValue = Time();
  edtDate->EditValue = Date();
  EditorDemoFrameManager()->Frames[bfcValueEditors]->SetDescription(SToolBarDescription);
}

void __fastcall TfrmMain::InitializeEditorStyles()
{
  TStrings* AItems = ((TcxComboBoxProperties*)(cbSelectEditType->Properties))->Items;
  AItems->BeginUpdate();
  try
  {
    for (int I = 0; I < GetRegisteredEditProperties()->Count; I++)
      AItems->Add(GetRegisteredEditProperties()->Descriptions[I]);
  }
  __finally
  {
    AItems->EndUpdate();
  };

  TStringList* AStyles = new TStringList;
  try
  {
    for (int I = 0; I < cxStyleRepository1->Count; I++)
      AStyles->Add(cxStyleRepository1->Items[I]->Name);
    InitializeStylesCombo(cbStyle, AStyles);
    InitializeStylesCombo(cbStyleEdit, AStyles);
  }
  __finally
  {
    delete AStyles;
  };
  cbSelectEditType->EditValue = "TextEdit";

  EditorDemoFrameManager()->Frames[bfcStyles]->SetDescription(SEditorStyles);
};

TdxRibbonTab* __fastcall TfrmMain::AddTab(TdxBar* AToolBar)
{
  TdxRibbonTab* Result = FRibbon->Tabs->Add();
  Result->Caption = TabCaptions[FRibbon->Tabs->Count - 1];
  Result->Tag = AToolBar->Tag;
  return(Result);
};

void __fastcall TfrmMain::AddDemoToolBars()
{
  int I;
  for (I = 0; I < DemoTabsCount; I++)
    AddTab(BarManager->Bars->Items[I]);
  for (I = 0; I < BarManager->Bars->Count; I++)
  {
    TdxBar* AItem = BarManager->Bars->Items[I];
    if ((AItem->Tag > 0)&&(AItem->Tag <= 6))
      FRibbon->Tabs->Items[BarManager->Bars->Items[I]->Tag - 1]->AddToolBar(BarManager->Bars->Items[I]);
  };
};

void __fastcall TfrmMain::AddToolBar(TdxBar* AToolBar)
{
  AddTab(AToolBar)->AddToolBar(AToolBar);
};

void __fastcall TfrmMain::InitializeRibbon()
{
  FRibbon = new TdxRibbon(frmMain);
  FRibbon->Parent = frmMain;
  FRibbon->SupportNonClientDrawing = false;
  FRibbon->Visible = false;
  FRibbon->BarManager = BarManager;
  cbRibbonFont->EditValue = "Tab Caption";
  FRibbon->ColorSchemeName = "Blue";

  AddDemoToolBars();

  AddToolBar(tlbEditorType);
  FRibbon->Tabs->Items[FRibbon->Tabs->Count - 1]->AddToolBar(tlbEditorStyles);
  FRibbon->Tabs->Items[FRibbon->Tabs->Count - 1]->AddToolBar(tlbPreview);

  AddToolBar(tlbRibbonOptions);
  FRibbon->Tabs->Items[FRibbon->Tabs->Count - 1]->AddToolBar(tlbColorScheme);
  
  AddToolBar(tlbHelp);
  FRibbon->OnTabChanged = RibbonTabChanged;
  FRibbon->ApplicationButton->Glyph->Assign(Image1->Picture->Bitmap);
  FRibbon->BarManager = 0;
  EditorDemoFrameManager()->Frames[bfcRibbonOptions]->SetDescription(SRibbonOptionsDescription);
};

void __fastcall TfrmMain::InitializeFrames()
{
  for (int I = 0; I < EditorDemoFrameManager()->FramesCount; I++)
  {
    EditorDemoFrameManager()->Frames[I]->Parent = Panel1;
    EditorDemoFrameManager()->Frames[I]->Align = alClient;
  }
};

void __fastcall TfrmMain::SynchronizeCalcValueEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    edtMoney->EditValue = AValue;
    edtCalculate->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeCheckGroupEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    chgSelectColor->EditValue = AValue;
    cbSelectColor->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeImageEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    edtBlobImage->EditValue = AValue;
    edtImage->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeMultilineTextEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
	edtBlob->EditValue = AValue;
	memMemo->EditValue = AValue;
	TStringStream* AStream = new TStringStream(dxVariantToString(AValue));
	try
    {
      memPopup->Lines->Clear();
      memPopup->Lines->LoadFromStream(AStream);
    }
    __finally
    {
      delete AStream;
    };
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizePathEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    scbSelectPath->EditValue = AValue;
    edtLastPath->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeRibbonFontEditors()
{
  FUpdateLock = true;
  try
  {
    int AFontIndex = ((TcxComboBoxProperties*)(cbRibbonFont->Properties))->Items->IndexOf(cbRibbonFont->EditValue);
    switch (AFontIndex)
    {
      case 0:
        {fncRibbonFontName->EditValue = FRibbon->Fonts->TabHeader->Name;
         break;
        };
      case 1:
        {fncRibbonFontName->EditValue = FRibbon->Fonts->Group->Name;
         break;
        };
      case 2:
        {fncRibbonFontName->EditValue = FRibbon->Fonts->GroupHeader->Name;
         break;
        };
    };

    AnsiString S = "";
    for (int ARibbonFont = 0; ARibbonFont < 3; ARibbonFont++)
      if (FRibbon->Fonts->AssignedFonts.Contains((TdxRibbonAssignedFont)ARibbonFont))
        S = S + '1';
      else
        S = S + '0';

    ccbAssignedRibbonFonts->EditValue = S;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeValueEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    seFontSize->EditValue = AValue;
    trbFontSize->EditValue = AValue;
    prbFontSize->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

void __fastcall TfrmMain::SynchronizeTextEditors(Variant AValue)
{
  FUpdateLock = true;
  try
  {
    lblCompanyName->Caption = AValue;
    beCompanyName->EditValue = AValue;
    edtCompanyName->EditValue = AValue;
  }
  __finally
  {
    FUpdateLock = false;
  };
};

bool __fastcall TfrmMain::IsItemControlSelected(TdxBarItem* ABarItem, int ALink)
{
  return((ABarItem->Links[ALink]->Control != 0) && (ABarItem->Links[ALink]->Control->IsSelected));
};

void __fastcall TfrmMain::clcFontColorChange(TObject *Sender)
{
  UpdateComboBoxesView();
};

void __fastcall TfrmMain::edtSiteChange(TObject *Sender)
{
  UpdateTextEditView();
};

void __fastcall TfrmMain::chbMonochromeChange(TObject *Sender)
{
  UpdateCheckBoxesView();
  tsMonochrome->EditValue = chbMonochrome->EditValue;
};

void __fastcall TfrmMain::scbSelectPathChange(TObject *Sender)
{
  if (!FUpdateLock)
  {
    FUpdateLock = true;
    try
    {
      AnsiString AValue;
      AValue = scbSelectPath->EditValue.AsType(varString);
      edtLastPath->Properties->BeginUpdate();
      try
      {
        TStrings* AMRUEditLookupItems = ((TcxMRUEditProperties*)(edtLastPath->Properties))->LookupItems;
        int AIndex = AMRUEditLookupItems->IndexOf(AValue);
        if (AIndex != -1)
          AMRUEditLookupItems->Move(AIndex, 0);
        else
          AMRUEditLookupItems->Insert(0, AValue);
        if (AMRUEditLookupItems->Count > 100)
          AMRUEditLookupItems->Delete(AMRUEditLookupItems->Count - 1);
      }
      __finally
      {
        edtLastPath->Properties->EndUpdate(false);
      }

      SynchronizePathEditors(AValue);
      UpdateComboBoxesView();
    }
    __finally
    {
      FUpdateLock = false;
    };
  };
};

void __fastcall TfrmMain::edtLastPathChange(TObject *Sender)
{
  if (!FUpdateLock)
  {
    SynchronizePathEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateComboBoxesView();
  };
};

void __fastcall TfrmMain::edtImageChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    SynchronizeImageEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateImageEditorsView();
  };
};

void __fastcall TfrmMain::reRichChange(TObject *Sender)
{
  TStringStream* AStream = new TStringStream(dxVariantToString(reRich->EditValue));
  try
  {
    TfrmMultiLineTextEditors* AFrame = dynamic_cast<TfrmMultiLineTextEditors*>(EditorDemoFrameManager()->Frames[bfcMultilineTextEditors]);
    AFrame->SetParameters(AStream, 0);
  }
  __finally
  {
    delete AStream;
  };
};

void __fastcall TfrmMain::memMemoChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    SynchronizeMultilineTextEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateMultilineTextEditorsView();
  }
};

void __fastcall TfrmMain::btnExitClick(TObject *Sender)
{
  frmMain->Close();
};

void __fastcall TfrmMain::btnShowDescriptionClick(TObject *Sender)
{
  lblDemoDescription->Visible = btnShowDescription->Down;
}

void __fastcall TfrmMain::btnToolBarDescriptionsClick(TObject *Sender)
{
  EditorDemoFrameManager()->SetDescriptionsVisible(btnToolBarDescriptions->Down);
};

void __fastcall TfrmMain::beCompanyNamePropertiesButtonClick(TObject *Sender,
  int AButtonIndex)
{
  ShowMessage("Company name: " + VarToStr(beCompanyName->EditValue));
};

void __fastcall TfrmMain::cbStyleEditChange(TObject *Sender)
{
  SetEditorStyle("StyleEdit", cbStyleEdit);
};

void __fastcall TfrmMain::SetEditorStyle(AnsiString APropName, TcxBarEditItem* AStyleSource)
{
  int AStyleIndex = ((TcxComboBoxProperties*)(AStyleSource->Properties))->Items->IndexOf(AStyleSource->EditValue);
  TcxStyle* AStyle = (TcxStyle*)(cxStyleRepository1->Items[AStyleIndex]);
  if (APropName == "Style")
   edtPreviewItem->Style = AStyle;
  else /* "StyleEdit" */
   edtPreviewItem->StyleEdit = AStyle;
};

void __fastcall TfrmMain::imcImagesChange(TObject *Sender)
{
  UpdateComboBoxesView();
};

void __fastcall TfrmMain::fncRibbonFontNameChange(TObject *Sender)
{
  if (! FUpdateLock)
  {
    int AFontIndex = ((TcxComboBoxProperties*)(cbRibbonFont->Properties))->Items->IndexOf(cbRibbonFont->EditValue);
    switch (AFontIndex)
    {
      case 0:
        {FRibbon->Fonts->TabHeader->Name = fncRibbonFontName->EditValue; break;}
      case 1:
        {FRibbon->Fonts->Group->Name = fncRibbonFontName->EditValue; break;}
      case 2:
        {FRibbon->Fonts->GroupHeader->Name = fncRibbonFontName->EditValue; break;}
    };
    SynchronizeRibbonFontEditors();
  };
}

void __fastcall TfrmMain::btnStandardClick(TObject *Sender)
{
  BarManager->Style = (TdxBarManagerStyle)(((TComponent*)(Sender))->Tag);
}

void __fastcall TfrmMain::btnBlueRibbonSchemeClick(TObject *Sender)
{
  FRibbon->ColorSchemeName = ((TdxBarLargeButton*)(Sender))->Caption;
}

void __fastcall TfrmMain::edtDateChange(TObject *Sender)
{
  UpdateValueEditorsView();
}

//---------------------------------------------------------------------------


void __fastcall TfrmMain::mdContactsCalcFields(TDataSet *DataSet)
{
  mdContacts->FieldByName("FullName")->Value = mdContacts->FieldByName("FirstName")->AsString + " " + mdContacts->FieldByName("LastName")->AsString;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::edtDatePropertiesGetDayOfWeekState(TObject *Sender, TDay ADayOfWeek,
  TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor)
{
  if ((ADayOfWeek == dSaturday) || (ADayOfWeek == dSunday))
    AFont->Color = clRed;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::edtDatePropertiesGetDayState(TObject *Sender, TDateTime ADate,
  TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor)
{
  Word AYear, AMonth, ADay, AWeekDay;
  AWeekDay = DayOfTheWeek(ADate);
  DecodeDate(ADate, AYear, AMonth, ADay);
  if ((ADay == 1) || (ADay == 15)) 
    AFont->Style = AFont->Style << fsBold;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::edtPopupPropertiesCloseUp(TObject *Sender)
{
  if (! FUpdateLock)
  {
    SynchronizeMultilineTextEditors(memPopup->Lines->Text);
    UpdateMultilineTextEditorsView();
  }
};
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbLookUpChange(TObject *Sender)
{
  TLocateOptions AOptions;
  mdContacts->Locate("ID", cbLookUp->EditValue, AOptions);
  UpdateComboBoxesView();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::chgSelectColorChange(TObject *Sender)
{
  if (!FUpdateLock)
  {
    SynchronizeCheckGroupEditors(((TcxBarEditItem*)(Sender))->EditValue);
    UpdateCheckBoxesView();
  };
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbFontSizePropertiesValidate(TObject *Sender,
      Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  //if (VarIsNull(DisplayValue))
  //   DisplayValue = ((TcxBarEditItem*)(Sender))->EditValue;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::tsMonochromeChange(TObject *Sender)
{
  UpdateCheckBoxesView();
  chbMonochrome->EditValue = tsMonochrome->EditValue;
}
//---------------------------------------------------------------------------

