//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoFrameManager.h"
#include <Math.hpp>
#include "cxGraphics.hpp"
#include "cxGeometry.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxGraphics"
#pragma link "cxGeometry"
#pragma resource "*.dfm"
TEditorDemoBaseFrame *EditorDemoBaseFrame;
TEditorDemoFrameManager* FInstance;
//---------------------------------------------------------------------------

void __fastcall TEditorDemoBaseFrame::DrawText(AnsiString AStrings[], TPaintBox* APaintBox, int AStringsCount)
{
  String S = "";
  TSize ASize = Size(0, 0);
  for (int I = 0; I <= (AStringsCount - 1); I++)
  {
    ASize.cx = Max(ASize.cx, APaintBox->Canvas->TextWidth(AStrings[I]));
    S = S + AStrings[I] + "\n";
  };
  ASize.cy = APaintBox->Canvas->TextHeight("Wg") * AStringsCount;
  TRect ARect = cxRectCenter(APaintBox->ClientRect, ASize);
  cxDrawText(THandle(APaintBox->Canvas->Handle), S, ARect, DT_WORDBREAK, -1);
}
//---------------------------------------------------------------------------
__fastcall TEditorDemoBaseFrame::TEditorDemoBaseFrame(TComponent* Owner)
	: TForm(Owner)
{

}
//---------------------------------------------------------------------------
bool __fastcall TEditorDemoBaseFrame::GetDescriptionVisible()
{
  return(lblFrameDescription->Visible);
}
//---------------------------------------------------------------------------
void __fastcall TEditorDemoBaseFrame::SetDescription(AnsiString ADescription)
{
  lblFrameDescription->Caption = ADescription;
};
//---------------------------------------------------------------------------
void __fastcall TEditorDemoBaseFrame::SetDescriptionVisible(bool AValue)
{
  lblFrameDescription->Visible = AValue;
};
//---------------------------------------------------------------------------
__fastcall TEditorDemoFrameManager::TEditorDemoFrameManager()
{
  FFrameList = new TList();
};
//---------------------------------------------------------------------------
__fastcall TEditorDemoFrameManager::~TEditorDemoFrameManager()
{
  delete FFrameList;
};
//---------------------------------------------------------------------------
TEditorDemoBaseFrame* __fastcall TEditorDemoFrameManager::GetFrame(int AIndex)
{
  TEditorDemoBaseFrame* Result;
  for(int I = 0; I <= (FramesCount - 1); I++)
  {
    Result = (TEditorDemoBaseFrame*)(FFrameList->Items[I]);
    if (Result->Tag == AIndex)
    {
      return(Result);
    };
  };
  return(0);
};
//---------------------------------------------------------------------------
void __fastcall TEditorDemoFrameManager::UpdateFrameColors(TColor AColor)
{
  for(int I = 0; I <= (FFrameList->Count - 1); I++)
    Frames[I]->Color = AColor;
};
//---------------------------------------------------------------------------
void __fastcall TEditorDemoFrameManager::AddFrame(TForm* AFrame, int AFrameId)
{
  FFrameList->Add(AFrame);
  AFrame->Tag = AFrameId;
};
//---------------------------------------------------------------------------
void __fastcall TEditorDemoFrameManager::SetDescriptionsVisible(bool AValue)
{
  for (int I = 0; I <= (FFrameList->Count - 1); I++)
    Frames[I]->SetDescriptionVisible(AValue);
};
//---------------------------------------------------------------------------
int __fastcall TEditorDemoFrameManager::GetFramesCount()
{
  return(FFrameList->Count);
};
//---------------------------------------------------------------------------
TEditorDemoFrameManager* EditorDemoFrameManager()
{
  if (!FInstance)
    FInstance = new TEditorDemoFrameManager();
  return FInstance;
}
//---------------------------------------------------------------------------
