//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DBTrPrnmain.h"
#include "dbtreeqr.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
TQRListForm *QRListForm;
//---------------------------------------------------------------------
// QuickReport simple list
// - Connect a datasource to the QuickReport component
// - Put QRDBText components on the Detail band
//---------------------------------------------------------------------
const
  PixelPerLevel = 50;

__fastcall TQRListForm::TQRListForm(TComponent* AOwner)
	: TForm(AOwner)
{
}

//---------------------------------------------------------------------
void __fastcall HideQRShape(TQRShape * qs)
{
   qs->Visible = false;
   qs->Left = -100;
   qs->Width = 0;
   qs->Height = 0;
}

//---------------------------------------------------------------------
void __fastcall TQRListForm::DataSource1DataChange(TObject *Sender,
	TField *Field)
{
  int Index;
  Index = FMain->DBTreePrintDataSet->FindField("Pr_id")->AsInteger;
 if ((Index >= 150) && (Index < 200)){
    FMain->ImageList1->GetBitmap(0, Image->Picture->Bitmap);
    return;
 }
 if (Index >= 200){
   FMain->ImageList1->GetBitmap(1, Image->Picture->Bitmap);
   return;
 }
 FMain->ImageList1->GetBitmap(2, Image->Picture->Bitmap);
}
//---------------------------------------------------------------------------
typedef TQRShape* PQRShape;
//---------------------------------------------------------------------------

void __fastcall TQRListForm::QuickReportBeforePrint(TCustomQuickRep *Sender,
      bool &PrintReport)
{
 int i;
 PQRShape qshp;

  ShapeCount = FMain->DBTreePrintDataSet->MaxLevelCount;
  ShapeList = new TList;
  ImageLeft = Image->Left;
  QTextLeft = QText->Left;
  ImageRectLeft = ImageRect->Left;
  for(i = 0; i <= ShapeCount; i ++){
    qshp = new TQRShape(this);
    qshp->Parent = Detail;
    qshp->ParentReport = QuickReport;
    qshp->Visible = false;
    if (i == 0)
      qshp->Shape = qrsHorLine;
    else qshp->Shape = qrsVertLine;
    qshp->Pen->Style = psDot;
    HideQRShape(qshp);
    ShapeList->Add(qshp);
   }
}
//---------------------------------------------------------------------------

void __fastcall TQRListForm::QuickReportAfterPrint(TObject *Sender)
{
 int i;
 PQRShape qshp;

 for( i = 0; i <= ShapeCount; i ++){
   qshp = PQRShape(ShapeList->Items[i]);
   qshp->Free();
 }
  ShapeList->Free();
}
//---------------------------------------------------------------------------
void __fastcall TQRListForm::DetailAfterPrint(TQRCustomBand *Sender,
      bool BandPrinted)
{
 int i;
 PQRShape qs;

 Image->Left = ImageLeft;
 QText->Left = QTextLeft;
 ImageRect->Left = ImageRectLeft;
 if(! ImageRect->Visible)
    ImageRect->Height = 9;
 ImageRect->Visible = true;
 for (i = 0; i <= ShapeCount; i ++){
    qs = PQRShape(ShapeList->Items[i]);
    HideQRShape(qs);
 }
}
//---------------------------------------------------------------------------

void __fastcall TQRListForm::DetailBeforePrint(TQRCustomBand *Sender,
      bool &PrintBand)
{
 int i, Level, inten;
 PQRShape qs;

 Level = FMain->DBTreePrintDataSet->FindField("dx$level")->AsInteger;
 ImageRect->Visible = FMain->DBTreePrintDataSet->FindField("dx$haschildren")->AsBoolean;
 inten = Level * PixelPerLevel;
 Image->Left = Image->Left + inten;
 QText->Left = QText->Left + inten;
 ImageRect->Left = ImageRect->Left + inten;
 qs = PQRShape(ShapeList->Items[0]);
 qs->Visible = true;
 if (ImageRect->Visible)
   qs->Left = ImageRect->Left + 1 + ImageRect->Width;
 else {
   qs->Left = ImageRect->Left + int(ImageRect->Width / 2) - PixelPerLevel;
   ImageRect->Height = 1;
 }

 qs->Width = Image->Left - qs->Left - 1;
 qs->Top =  ImageRect->Top + int(ImageRect->Height / 2);
 for (i = 0; i < Level; i ++) {
   qs = PQRShape(ShapeList->Items[i + 1]);
   qs->Visible = true;
   qs->Left = ImageRectLeft + int(ImageRect->Width / 2) + i * PixelPerLevel;
   qs->Height = Detail->Height;
 }
  FMain->ImageList1->GetBitmap(0, Image->Picture->Bitmap);
}
//---------------------------------------------------------------------------

