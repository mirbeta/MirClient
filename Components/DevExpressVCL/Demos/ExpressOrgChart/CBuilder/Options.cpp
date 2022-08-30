//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Options.h"
//---------------------------------------------------------------------------
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TOptionsForm *OptionsForm;
//---------------------------------------------------------------------------
__fastcall TOptionsForm::TOptionsForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TOptionsForm::FormActivate(TObject *Sender)
{
  if (MainForm->PC->ActivePage == MainForm->tsOrgChart) {
    Caption = "dxOrgChart Options";
    cbLeft->Checked     = MainForm->Tree->EditMode.Contains(emLeft);
    cbCenter->Checked   = MainForm->Tree->EditMode.Contains(emCenter);
    cbRight->Checked    = MainForm->Tree->EditMode.Contains(emRight);
    cbVCenter->Checked  = MainForm->Tree->EditMode.Contains(emVCenter);
    cbWrap->Checked     = MainForm->Tree->EditMode.Contains(emWrap);
    cbUpper->Checked    = MainForm->Tree->EditMode.Contains(emUpper);
    cbLower->Checked    = MainForm->Tree->EditMode.Contains(emLower);
    cbGrow->Checked     = MainForm->Tree->EditMode.Contains(emGrow);

    cbSelect->Checked   = MainForm->Tree->Options.Contains(ocSelect);
    cbFocus->Checked    = MainForm->Tree->Options.Contains(ocFocus);
    cbButtons->Checked  = MainForm->Tree->Options.Contains(ocButtons);
    cbEdit->Checked     = MainForm->Tree->Options.Contains(ocEdit);
    cbCanDrag->Checked  = MainForm->Tree->Options.Contains(ocCanDrag);
    cbShowDrag->Checked = MainForm->Tree->Options.Contains(ocShowDrag);
    cbInsDel->Checked   = MainForm->Tree->Options.Contains(ocInsDel);

	seX->Text = IntToStr(MainForm->Tree->IndentX);
	seY->Text = IntToStr(MainForm->Tree->IndentY);
    seLineWidth->Text = IntToStr(MainForm->Tree->LineWidth);
    cbShowImages->Checked = ! (MainForm->Tree->Images == NULL);
  };

  if (MainForm->PC->ActivePage == MainForm->tsDBOrgChart) {
    Caption = "dxDBOrgChart Options";
    cbLeft->Checked     = MainForm->DBTree->EditMode.Contains(emLeft);
    cbCenter->Checked   = MainForm->DBTree->EditMode.Contains(emCenter);
    cbRight->Checked    = MainForm->DBTree->EditMode.Contains(emRight);
    cbVCenter->Checked  = MainForm->DBTree->EditMode.Contains(emVCenter);
    cbWrap->Checked     = MainForm->DBTree->EditMode.Contains(emWrap);
    cbUpper->Checked    = MainForm->DBTree->EditMode.Contains(emUpper);
    cbLower->Checked    = MainForm->DBTree->EditMode.Contains(emLower);
    cbGrow->Checked     = MainForm->DBTree->EditMode.Contains(emGrow);

    cbSelect->Checked   = MainForm->DBTree->Options.Contains(ocSelect);
    cbFocus->Checked    = MainForm->DBTree->Options.Contains(ocFocus);
    cbButtons->Checked  = MainForm->DBTree->Options.Contains(ocButtons);
    cbEdit->Checked     = MainForm->DBTree->Options.Contains(ocEdit);
    cbCanDrag->Checked  = MainForm->DBTree->Options.Contains(ocCanDrag);
    cbShowDrag->Checked = MainForm->DBTree->Options.Contains(ocShowDrag);
    cbInsDel->Checked   = MainForm->DBTree->Options.Contains(ocInsDel);

	seX->Text = IntToStr(MainForm->DBTree->IndentX);
	seY->Text = IntToStr(MainForm->DBTree->IndentY);
    seLineWidth->Text = IntToStr(MainForm->DBTree->LineWidth);
    cbShowImages->Checked = ! (MainForm->DBTree->Images == NULL);
  };
}
//---------------------------------------------------------------------------
void __fastcall TOptionsForm::BitBtn2Click(TObject *Sender)
{
  if (MainForm->PC->ActivePage == MainForm->tsOrgChart) {
    if (cbLeft->Checked) MainForm->Tree->EditMode << emLeft;
    else MainForm->Tree->EditMode >> emLeft;
    if (cbCenter->Checked) MainForm->Tree->EditMode << emCenter;
    else MainForm->Tree->EditMode >> emCenter;
    if (cbRight->Checked) MainForm->Tree->EditMode << emRight;
    else MainForm->Tree->EditMode >> emRight;
    if (cbVCenter->Checked) MainForm->Tree->EditMode << emVCenter;
    else MainForm->Tree->EditMode >> emVCenter;
    if (cbWrap->Checked) MainForm->Tree->EditMode << emWrap;
    else MainForm->Tree->EditMode >> emWrap;
    if (cbUpper->Checked) MainForm->Tree->EditMode << emUpper;
    else MainForm->Tree->EditMode >> emUpper;
    if (cbLower->Checked) MainForm->Tree->EditMode << emLower;
    else MainForm->Tree->EditMode >> emLower;
    if (cbGrow->Checked) MainForm->Tree->EditMode << emGrow;
    else MainForm->Tree->EditMode >> emGrow;

    if (cbSelect->Checked) MainForm->Tree->Options << ocSelect;
    else MainForm->Tree->Options >> ocSelect;
    if (cbFocus->Checked) MainForm->Tree->Options << ocFocus;
    else MainForm->Tree->Options >> ocFocus;
    if (cbButtons->Checked) MainForm->Tree->Options << ocButtons;
    else MainForm->Tree->Options >> ocButtons;
    if (cbEdit->Checked) MainForm->Tree->Options << ocEdit;
    else MainForm->Tree->Options >> ocEdit;
    if (cbCanDrag->Checked) MainForm->Tree->Options << ocCanDrag;
    else MainForm->Tree->Options >> ocCanDrag;
    if (cbShowDrag->Checked) MainForm->Tree->Options << ocShowDrag;
    else MainForm->Tree->Options >> ocShowDrag;
    if (cbInsDel->Checked) MainForm->Tree->Options << ocInsDel;
    else MainForm->Tree->Options >> ocInsDel;
    if (cbShowImages->Checked) MainForm->Tree->Images = MainForm->ilTree;
    else MainForm->Tree->Images = NULL;

	MainForm->Tree->IndentX = StrToInt(seX->Text);
	MainForm->Tree->IndentY = StrToInt(seY->Text);
    MainForm->Tree->LineWidth = StrToInt(seLineWidth->Text);
    MainForm->Tree->Refresh();
  };

  if (MainForm->PC->ActivePage == MainForm->tsDBOrgChart) {
    if (cbLeft->Checked) MainForm->DBTree->EditMode << emLeft;
    else MainForm->DBTree->EditMode >> emLeft;
    if (cbCenter->Checked) MainForm->DBTree->EditMode << emCenter;
    else MainForm->DBTree->EditMode >> emCenter;
    if (cbRight->Checked) MainForm->DBTree->EditMode << emRight;
    else MainForm->DBTree->EditMode >> emRight;
    if (cbVCenter->Checked) MainForm->DBTree->EditMode << emVCenter;
    else MainForm->DBTree->EditMode >> emVCenter;
    if (cbWrap->Checked) MainForm->DBTree->EditMode << emWrap;
    else MainForm->DBTree->EditMode >> emWrap;
    if (cbUpper->Checked) MainForm->DBTree->EditMode << emUpper;
    else MainForm->DBTree->EditMode >> emUpper;
    if (cbLower->Checked) MainForm->DBTree->EditMode << emLower;
    else MainForm->DBTree->EditMode >> emLower;
    if (cbGrow->Checked) MainForm->DBTree->EditMode << emGrow;
    else MainForm->DBTree->EditMode >> emGrow;

    if (cbSelect->Checked) MainForm->DBTree->Options << ocSelect;
    else MainForm->DBTree->Options >> ocSelect;
    if (cbFocus->Checked) MainForm->DBTree->Options << ocFocus;
    else MainForm->DBTree->Options >> ocFocus;
    if (cbButtons->Checked) MainForm->DBTree->Options << ocButtons;
    else MainForm->DBTree->Options >> ocButtons;
    if (cbEdit->Checked) MainForm->DBTree->Options << ocEdit;
    else MainForm->DBTree->Options >> ocEdit;
    if (cbCanDrag->Checked) MainForm->DBTree->Options << ocCanDrag;
    else MainForm->DBTree->Options >> ocCanDrag;
    if (cbShowDrag->Checked) MainForm->DBTree->Options << ocShowDrag;
    else MainForm->DBTree->Options >> ocShowDrag;
    if (cbInsDel->Checked) MainForm->DBTree->Options << ocInsDel;
    else MainForm->DBTree->Options >> ocInsDel;
    if (cbShowImages->Checked) MainForm->DBTree->Images = MainForm->ilTree;
    else MainForm->DBTree->Images = NULL;

	MainForm->DBTree->IndentX = StrToInt(seX->Text);
	MainForm->DBTree->IndentY = StrToInt(seY->Text);
    MainForm->DBTree->LineWidth = StrToInt(seLineWidth->Text);
    MainForm->DBTree->Refresh();
  };

  Close();
}
//---------------------------------------------------------------------------
