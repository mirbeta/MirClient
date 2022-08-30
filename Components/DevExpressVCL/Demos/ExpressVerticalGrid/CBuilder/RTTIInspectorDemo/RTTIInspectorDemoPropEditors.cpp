//---------------------------------------------------------------------------


#pragma hdrstop

#include "RTTIInspectorDemoPropEditors.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxOI"
#pragma link "cxEdit"
#pragma link "cxImageComboBox"
//---------------------------------------------------------------------------

const String sNoImage = "no image";

void __fastcall TcxImageIndexProperty::AdjustInnerEditProperties(Cxedit::TcxCustomEditProperties* AProperties)
{
  TcxImageComboBoxProperties *AImageComboBoxProperties =
    (TcxImageComboBoxProperties*)AProperties;
  TCustomImageList *AImages = GetImages();
  TcxImageComboBoxItem *AComboBoxItem;
  if (AImages != NULL){
    if (AImages->Height == 16)
      AImageComboBoxProperties->Images = AImages;
    else
      AImageComboBoxProperties->LargeImages = AImages;
    AImageComboBoxProperties->Items->Clear();
    for (int I = 0; I < AImages->Count; I++){
      AComboBoxItem = (TcxImageComboBoxItem*)AImageComboBoxProperties->Items->Add();
      AComboBoxItem->ImageIndex = I;
      AComboBoxItem->Value = I;
      AComboBoxItem->Description = IntToStr(I);
    }
  }
  AComboBoxItem = (TcxImageComboBoxItem*)AImageComboBoxProperties->Items->Add();
  AComboBoxItem->ImageIndex = -1;
  AComboBoxItem->Value = (int)-1;
  AComboBoxItem->Description = sNoImage;
  AImageComboBoxProperties->OnEditValueChanged = NULL;
}
//---------------------------------------------------------------------------

TCustomImageList* __fastcall TcxImageIndexProperty::GetImages(void)
{
  TCustomImageList* Result = NULL;
  if (dynamic_cast<TcxCustomEditorRowProperties *>(GetComponent(0)) != 0)
     Result = ((TcxEditorRowProperties*)GetComponent(0))->Row->VerticalGrid->Images;
  return Result;
}
//---------------------------------------------------------------------------

TcxPropertyAttributes __fastcall TcxImageIndexProperty::GetAttributes(void)
{
  return TcxPropertyAttributes()<<ipaMultiSelect<<ipaAutoUpdate;
}
//---------------------------------------------------------------------------

void __fastcall TcxImageIndexProperty::SetValue(const AnsiString Value)
{
  if (Value == sNoImage)
    SetOrdValue(-1);
  else
    TcxIntegerProperty::SetValue(Value);
}
//---------------------------------------------------------------------------







