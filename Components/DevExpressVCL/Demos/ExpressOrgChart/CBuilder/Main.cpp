//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma link "dxorgchr"
#pragma link "dxdborgc"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxorgced"
#pragma link "cxClasses"
#pragma resource "*.dfm"
TMainForm *MainForm;
bool FChange;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TMainForm::DBGrid1DrawColumnCell(TObject *Sender,
	const TRect &Rect, int DataCol, TColumn *Column, TGridDrawState State)
{
  if (Column->FieldName == "COLOR") {
	DBGrid1->Canvas->Brush->Color = (TColor)Table1->FieldByName("COLOR")->AsInteger;
    DBGrid1->Canvas->FillRect(Rect);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DBGrid1ColEnter(TObject *Sender)
{
  if (DBGrid1->SelectedField->FieldName == "COLOR")
	DBGrid1->Options = DBGrid1->Options << dgEditing;
  else
	DBGrid1->Options = DBGrid1->Options >> dgEditing;
  DBGrid1->Refresh();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Exit1Click(TObject *Sender)
{
  Close();	
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::AddNode1Click(TObject *Sender)
{
  TdxOcNode *Node;

  if (PC->ActivePage == tsDBOrgChart) {
    Table1->DisableControls();
    if (DBTree->Selected != NULL)
      Node = DBTree->Insert(DBTree->Selected, NULL);
    else Node = DBTree->Add(NULL, NULL);
    Node->Text = "New topic";
    Node->Color = clWhite;
    Node->Shape = shRectangle;
    DBTree->Selected = Node;
    Table1->EnableControls();
  };

  if (PC->ActivePage == tsOrgChart) {
    if (Tree->Selected != NULL) 
      Node = Tree->Insert(Tree->Selected, NULL);
    else Node = Tree->Add(NULL, NULL);
    Node->Text = "New topic";
    Node->Color = clWhite;
    Node->Shape = shRectangle;
    Tree->Selected = Node;
  };
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::AddChildeNode1Click(TObject *Sender)
{
  TdxOcNode *Node;

  if (PC->ActivePage == tsOrgChart) {
	Table1->DisableControls();
	if (Tree->Selected != NULL)
	  Node = Tree->AddChild(Tree->Selected, NULL);
	else Node = Tree->Add(NULL, NULL);
	Node->Text = "New topic";
	Node->Color = clWhite;
	Node->Shape = shRectangle;
	if (Tree->Selected != NULL) Tree->Selected->Expanded = true;
	Tree->Selected = Node;
    Table1->EnableControls();
  };

  if (PC->ActivePage == tsDBOrgChart) {
    if (DBTree->Selected != NULL)
      Node = DBTree->AddChild(DBTree->Selected, NULL);
    else Node = DBTree->Add(NULL, NULL);
    Node->Text = "New topic";
    Node->Color = clWhite;
    Node->Shape = shRectangle;
    if (DBTree->Selected != NULL) DBTree->Selected->Expanded = true;
    DBTree->Selected = Node;
  };

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::RenameNode1Click(TObject *Sender)
{
  if (PC->ActivePage == tsOrgChart)
	if (Tree->Selected != NULL)
	  Tree->ShowEditor();

  if (PC->ActivePage == tsDBOrgChart)
    if (DBTree->Selected != NULL)
      DBTree->ShowEditor();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DeleteNode1Click(TObject *Sender)
{
  if (PC->ActivePage == tsOrgChart)
    if (Tree->Selected != NULL)
      Tree->Delete(Tree->Selected);

  if (PC->ActivePage == tsDBOrgChart)
    if (DBTree->Selected != NULL)
      DBTree->Delete(DBTree->Selected);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  Table1->Open();
  DBTree->WidthFieldName  = "Width";
  DBTree->HeightFieldName = "Height";
  DBTree->ChAlignFieldName = "Align_num";
  DBTree->ImAlignFieldName = "ImageAlign_num";
  DBTree->ShapeFieldName = "Type_num";
  DBTree->ColorFieldName = "Color";
  PCChange(PC);
  FChange = true;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::PCChange(TObject *Sender)
{
  if (PC->ActivePage == tsOrgChart) {
    ItZoom->Checked = Tree->Zoom;
    ItRotated->Checked = Tree->Rotated;
    ItAnimated->Checked = Tree->Options.Contains(ocAnimate);
	It3D->Checked = Tree->Options.Contains(ocRect3D);
	miAntialiasing->Checked = Tree->Antialiasing;
  };
  if (PC->ActivePage == tsDBOrgChart) {
	ItZoom->Checked = DBTree->Zoom;
	ItRotated->Checked = DBTree->Rotated;
	ItAnimated->Checked = DBTree->Options.Contains(ocAnimate);
	It3D->Checked = DBTree->Options.Contains(ocRect3D);
	miAntialiasing->Checked = DBTree->Antialiasing;
  };

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ItZoomClick(TObject *Sender)
{
 ((TMenuItem*)Sender)->Checked = ! ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsOrgChart)
   Tree->Zoom = ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsDBOrgChart)
   DBTree->Zoom = ((TMenuItem*)Sender)->Checked;

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ItRotatedClick(TObject *Sender)
{
 ((TMenuItem*)Sender)->Checked = ! ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsOrgChart)
   Tree->Rotated = ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsDBOrgChart)
   DBTree->Rotated = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ItAnimatedClick(TObject *Sender)
{
 ((TMenuItem*)Sender)->Checked = ! ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsOrgChart)
 {
   if (((TMenuItem*)Sender)->Checked)
     Tree->Options = Tree->Options << ocAnimate;
   else
     Tree->Options = Tree->Options >> ocAnimate;
   Tree->Refresh();
 };
 if (PC->ActivePage == tsDBOrgChart)
 {
   if (((TMenuItem*)Sender)->Checked)
     DBTree->Options = DBTree->Options << ocAnimate;
   else
     DBTree->Options = DBTree->Options >> ocAnimate;
   DBTree->Refresh();
 }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::It3DClick(TObject *Sender)
{
 ((TMenuItem*)Sender)->Checked = ! ((TMenuItem*)Sender)->Checked;
 if (PC->ActivePage == tsOrgChart) {
   if (((TMenuItem*)Sender)->Checked)
     Tree->Options = Tree->Options << ocRect3D;
   else
     Tree->Options = Tree->Options >> ocRect3D;
   Tree->Refresh();
 };

 if (PC->ActivePage == tsDBOrgChart) {
   if (((TMenuItem*)Sender)->Checked)
     DBTree->Options = DBTree->Options << ocRect3D;
   else
     DBTree->Options = DBTree->Options >> ocRect3D;
   DBTree->Refresh();
 };
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ItFullExpandClick(TObject *Sender)
{
  if (PC->ActivePage == tsOrgChart)
	Tree->FullExpand();
  if (PC->ActivePage == tsDBOrgChart)
    DBTree->FullExpand();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ItFullCollapseClick(TObject *Sender)
{
  if (PC->ActivePage == tsOrgChart)
	Tree->FullCollapse();
  if (PC->ActivePage == tsDBOrgChart)
    DBTree->FullCollapse();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DBTreeCreateNode(TObject *Sender, TdxOcNode *Node)
{
  if (Table1->FindField("Width")->AsInteger > 50)
	Node->Width = Table1->FindField("Width")->AsInteger;
  if (Table1->FindField("Height")->AsInteger > 50)
    Node->Height = Table1->FindField("Height")->AsInteger;
  Node->Shape = GetShape(Table1->FindField("Type")->AsString);
  Node->Color = (TColor)Table1->FindField("Color")->AsInteger;
  Node->ChildAlign = GetNodeAlign(Table1->FindField("Align")->AsString);
  Node->ImageAlign = GetImageAlign(Table1->FindField("ImageAlign")->AsString);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Table1AfterInsert(TDataSet *DataSet)
{
  Table1->FindField("Height")->AsInteger = DBTree->DefaultNodeHeight;
  Table1->FindField("Width")->AsInteger = DBTree->DefaultNodeWidth;
  Table1->FindField("Type")->AsString = "Rectangle";
  Table1->FindField("Color")->AsInteger = clWhite;
  Table1->FindField("Image")->AsInteger = -1;
  Table1->FindField("ImageAlign")->AsString = "Left-Top";
  Table1->FindField("Align")->AsString = "Center";
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DBGrid1DblClick(TObject *Sender)
{
  if (((TDBGrid*)Sender)->SelectedField->FieldName == "COLOR")
	if (ColorDialog->Execute())
	{
	  Table1->Edit();
	  Table1->FieldByName("COLOR")->AsInteger = ColorDialog->Color;
      Table1->Post();
      DBTree->Selected->Color = ColorDialog->Color;
    };
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::DBGrid1KeyDown(TObject *Sender, WORD &Key,
	TShiftState Shift)
{
  if (Key == VK_RETURN) {
    Key = 0;
    DBGrid1DblClick(Sender);
  };
  if (Key == VK_DOWN) {
    Key = 0;
    ((TDBGrid*)Sender)->DataSource->DataSet->Next();
  };
}
//---------------------------------------------------------------------------
TdxOcShape __fastcall TMainForm::GetShape(AnsiString ShapeName)
{
  AnsiString const ShapeArray[4] = {"Rectangle", "Round Rect", "Ellipse", "Diamond"};

  for(int i=0; i<4; i++)
	if (AnsiUpperCase(ShapeArray[i]) == AnsiUpperCase(ShapeName))
	  return((TdxOcShape(i)));
  return((TdxOcShape(0)));
};
//---------------------------------------------------------------------------

void __fastcall TMainForm::TableTYPEChange(TField *Sender)
{
  if (Table1->State == dsEdit)
    DBTree->Selected->Shape = GetShape(Sender->AsString);
}
//---------------------------------------------------------------------------
TdxOcNodeAlign __fastcall TMainForm::GetNodeAlign(AnsiString AlignName)
{
 AnsiString const AlignArray[3] = {"Left", "Center", "Right"};

 for(int i=0; i< 3; i++)
   if (AnsiUpperCase(AlignArray[i]) == AnsiUpperCase(AlignName))
     return(TdxOcNodeAlign(i));
 return(TdxOcNodeAlign(0));
};
//---------------------------------------------------------------------------

void __fastcall TMainForm::TableALIGNChange(TField *Sender)
{
  if (Table1->State == dsEdit)
    DBTree->Selected->ChildAlign = GetNodeAlign(Sender->AsString);
}
//---------------------------------------------------------------------------
TdxOcImageAlign __fastcall TMainForm::GetImageAlign(AnsiString AlignName)
{
 AnsiString const AlignArray[13] = {
   "None",
   "Left-Top", "Left-Center", "Left-Bottom",
   "Right-Top", "Right-Center", "Right-Bottom",
   "Top-Left", "Top-Center", "Top-Right",
   "Bottom-Left", "Bottom-Center", "Bottom-Right"
   };
  for(int i=0; i< 13; i++)
    if (AnsiUpperCase(AlignArray[i]) == AnsiUpperCase(AlignName))
      return(TdxOcImageAlign(i));
  return(TdxOcImageAlign(0));
};
//---------------------------------------------------------------------------

void __fastcall TMainForm::TableIMAGEALIGNChange(TField *Sender)
{
  if (Table1->State == dsEdit)
    DBTree->Selected->ImageAlign = GetImageAlign(Sender->AsString);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Options1Click(TObject *Sender)
{
  OptionsForm->ShowModal();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TreeCreateNode(TObject *Sender, TdxOcNode *Node)
{
	Node->Shape = shRectangle;
    Node->Color = clWhite;
    Node->ChildAlign = caCenter;
    Node->ImageAlign = iaLT;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Button1Click(TObject *Sender)
{
  ShowOrgChartEditor(Tree);
  PCChange(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miAntialiasingClick(TObject *Sender)
{
	((TMenuItem*)Sender)->Checked = ! ((TMenuItem*)Sender)->Checked;
	if (PC->ActivePage == tsOrgChart)
	{
		Tree->Antialiasing = ((TMenuItem*)Sender)->Checked;
		Tree->Refresh();
	};

	if (PC->ActivePage == tsDBOrgChart) 
	{
		DBTree->Antialiasing = ((TMenuItem*)Sender)->Checked;
		DBTree->Refresh();
	};
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::miKindNativeClick(TObject* Sender)
{
	int ATag = ((TMenuItem*)Sender)->Tag;
	cxLookAndFeelController->NativeStyle = ATag == 4;
	if (ATag < 4) cxLookAndFeelController->Kind = (TcxLookAndFeelKind)ATag;
}
void __fastcall TMainForm::Table1CalcFields(TDataSet *DataSet)
{
  if (DataSet->FieldByName("TYPE")->AsString == "Rectangle")
	DataSet->FieldByName("Type_num")->AsInteger = 0;
  if (DataSet->FieldByName("TYPE")->AsString == "Round Rect")
	DataSet->FieldByName("Type_num")->AsInteger = 1;
  if (DataSet->FieldByName("TYPE")->AsString == "Ellipse")
	DataSet->FieldByName("Type_num")->AsInteger = 2;
  if (DataSet->FieldByName("TYPE")->AsString == "Diamond")
	DataSet->FieldByName("Type_num")->AsInteger = 3;

  if (DataSet->FieldByName("ALIGN")->AsString == "Left")
	DataSet->FieldByName("Align_num")->AsInteger = 0;
  if (DataSet->FieldByName("ALIGN")->AsString == "Center")
	DataSet->FieldByName("Align_num")->AsInteger = 1;
  if (DataSet->FieldByName("ALIGN")->AsString == "Right")
	DataSet->FieldByName("Align_num")->AsInteger = 2;

  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "None")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 0;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Left-Top")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 1;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Left-Center")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 2;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Left-Bottom")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 3;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Right-Top")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 4;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Right-Center")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 5;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Right-Bottom")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 6;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Top-Left")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 7;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Top-Center")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 8;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Top-Right")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 9;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Bottom-Left")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 10;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Bottom-Center")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 11;
  if (DataSet->FieldByName("IMAGEALIGN")->AsString == "Bottom-Right")
	DataSet->FieldByName("ImageAlign_num")->AsInteger = 12;
}
//---------------------------------------------------------------------------

