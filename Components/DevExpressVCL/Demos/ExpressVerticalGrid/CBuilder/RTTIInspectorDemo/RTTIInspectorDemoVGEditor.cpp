//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RTTIInspectorDemoVGEditor.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxVGrid"
#pragma resource "*.dfm"

class TcxCustomVerticalGridAccess : public TcxCustomVerticalGrid
{
public:
  __property Customizing;
	virtual TcxCustomRowClass __fastcall GetEditorRowClass(void);
	virtual TcxCustomRowClass __fastcall GetMultiEditorRowClass(void);
};

class TcxVerticalGridCustomizingAccess : public TcxVerticalGridCustomizing
{
public:
  __property ShowCategoryButtons;
};

const String SSubStr = "Tcx";

TcxVerticalGridEditor *VerticalGridEditor;

TcxVerticalGridEditor *GetVerticalGridEditor(TcxCustomVerticalGrid *AVerticalGrid,
  TNotifyEvent AEvent)
{
  if (VerticalGridEditor == NULL){
    VerticalGridEditor = new TcxVerticalGridEditor(Application);
    try{
      VerticalGridEditor->VerticalGrid = AVerticalGrid;
      VerticalGridEditor->OnObjectSelected = AEvent;
      VerticalGridEditor->InitFormEditor();
    }
    catch(...){
      delete VerticalGridEditor;
      VerticalGridEditor = NULL;
    }
  }
  return VerticalGridEditor;
}

//---------------------------------------------------------------------------

bool IsComponentNameExist(TcxCustomVerticalGrid *AVerticalGrid, String ANewName)
{
  for (int I = 0; I < AVerticalGrid->Parent->Parent->ComponentCount; I++)
    if (CompareText(AVerticalGrid->Parent->Parent->Components[I]->Name, ANewName) == 0)
      return true;
  return false;
}

void GenVerticalGridRowName(TcxCustomVerticalGrid *AVerticalGrid,
  TcxCustomRow *ARow)
{
  String S1 = ARow->ClassName();
  String S, ARowName;
  int I = S1.Pos(SSubStr);
  if (I != 0)
    S = S1.SubString(I + SSubStr.Length(), S1.Length());
  I = 0;
  do{
    I++;
    ARowName = AVerticalGrid->Name + S + IntToStr(I);
  }while(IsComponentNameExist(AVerticalGrid, ARowName));
  ARow->Name = ARowName;
}

//---------------------------------------------------------------------------
__fastcall TcxVerticalGridEditor::TcxVerticalGridEditor(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::lbRowsClick(TObject *Sender)
{
  if (lbRows->ItemIndex != -1)
    DoObjectSelected(lbRows->Items->Objects[lbRows->ItemIndex]);
  UpdateButtons();
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btCategoryClick(TObject *Sender)
{
  AddRow(__classid(TcxCategoryRow));
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btEditorClick(TObject *Sender)
{
  AddRow(((TcxCustomVerticalGridAccess*)VerticalGrid)->GetEditorRowClass());
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btMultiEditorClick(TObject *Sender)
{
  AddRow(((TcxCustomVerticalGridAccess*)VerticalGrid)->GetMultiEditorRowClass());
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btDeleteClick(TObject *Sender)
{
  if (lbRows->ItemIndex != -1){
    void *AItem = NULL;
    if (lbRows->ItemIndex != -1){
      if (!lbRows->Selected[lbRows->ItemIndex])
        AItem = lbRows->Items->Objects[lbRows->ItemIndex];
      else{
        for (int I = lbRows->ItemIndex + 1; I < lbRows->Items->Count; I++)
          if (!lbRows->Selected[I]){
            AItem = lbRows->Items->Objects[I];
            break;
          }
        for (int I = lbRows->ItemIndex - 1; I >= 0; I--)
          if (!lbRows->Selected[I]){
            AItem = lbRows->Items->Objects[I];
            break;
          }
      }
    }
    DoObjectSelected(NULL);
    delete (TcxCustomRow*)lbRows->Items->Objects[lbRows->ItemIndex];
    UpdateItems();
    SelectItem(AItem);
    lbRowsClick(NULL);
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::btClearClick(TObject *Sender)
{
  DoObjectSelected(NULL);
  VerticalGrid->ClearRows();
  UpdateItems();
  UpdateButtons();
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::FormActivate(TObject *Sender)
{
  UpdateButtons();
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::miEditorClick(TObject *Sender)
{
  btEditorClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::miCategoryClick(TObject *Sender)
{
  btCategoryClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::miMultieditorClick(TObject *Sender)
{
  btMultiEditorClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::miDeleteClick(TObject *Sender)
{
  btDeleteClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::miClearAllClick(TObject *Sender)
{
  btClearClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  VerticalGridEditor = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::ShowVerticalGridLayoutEditorClick(TObject *Sender)
{
  TcxCustomVerticalGridAccess *AVGrid = (TcxCustomVerticalGridAccess*)VerticalGrid;
  ((TcxVerticalGridCustomizingAccess*)AVGrid->Customizing)->ShowCategoryButtons = true;
  AVGrid->Customizing->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::FormClose(TObject *Sender, TCloseAction &Action)
{
  ((TcxCustomVerticalGridAccess*)VerticalGrid)->Customizing->Visible = false;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::AddRow(TMetaClass* ARowClass)
{
  if (ARowClass != NULL){
    TcxCustomRow *ARow = VerticalGrid->Add(ARowClass);
    GenVerticalGridRowName(VerticalGrid, ARow);
    UpdateItems();
    SelectItem(ARow);
    UpdateButtons();
  }
}
//---------------------------------------------------------------------------

TcxCustomVerticalGrid* __fastcall TcxVerticalGridEditor::GetVerticalGrid(void)
{
  return FVerticalGrid;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::UpdateButtons()
{
  btDelete->Enabled = lbRows->ItemIndex != -1;
  miDelete->Enabled = btDelete->Enabled;
  btClear->Enabled = lbRows->Items->Count > 0;
  miClearAll->Enabled = btClear->Enabled;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::UpdateItems()
{
  int AItemIndex = lbRows->ItemIndex;
  try{
    lbRows->Items->Clear();
    for (int I = 0; I < VerticalGrid->Rows->Count; I++)
      lbRows->Items->AddObject(VerticalGrid->Rows->Items[I]->Name,
        VerticalGrid->Rows->Items[I]);
  }
  __finally{
    if (AItemIndex <= lbRows->Items->Count - 1)
      lbRows->ItemIndex = AItemIndex;
    else
      lbRows->ItemIndex = lbRows->Items->Count - 1;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::SetVerticalGrid(TcxCustomVerticalGrid *Value)
{
  FVerticalGrid = Value;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::SelectItem(void *AItem)
{
  int AItemIndex = lbRows->Items->IndexOfObject((TObject*)AItem);
  lbRows->ItemIndex = AItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::InitFormEditor()
{
  UpdateItems();
  UpdateButtons();
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::DoObjectSelected(TObject *Sender)
{
  if (FOnObjectSelected != NULL)
    FOnObjectSelected(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TcxVerticalGridEditor::DoItemsModified()
{
  UpdateItems();
}
//---------------------------------------------------------------------------


