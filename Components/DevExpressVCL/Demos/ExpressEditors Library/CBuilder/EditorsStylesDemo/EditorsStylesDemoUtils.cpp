//---------------------------------------------------------------------------


#pragma hdrstop

#include "EditorsStylesDemoConvert.h"
#include "EditorsStylesDemoUtils.h"
#include "EditorsStylesDemoPlanets.h"
#include "EditorsStylesDemoNoteBook.h"
#include "EditorsStylesDemoIssues.h"
#include "EditorsStylesDemoRichEdit.h"
#include "EditorsStylesDemoStylesPalette.h"
#include "Jpeg.hpp"

//---------------------------------------------------------------------------

#pragma package(smart_init)

TEditorsStylesDemoBaseFrame* __fastcall CreateFrameByID(int AID)
{
  TEditorsStylesDemoBaseFrame* AFrame;
  switch (AID) {
    case TextProcessingFrameID: {
      AFrame = new TEditorsStylesDemoRichEditFrame(NULL);
      break;
    }
   case ImageProcessingFrameID: {
      AFrame = new TEditorsStylesDemoConvertFrame(NULL);
      break;
    }
   case SolarSystemFrameID: {
     AFrame = new TEditorsStylesDemoPlanetsFrame(NULL);
     break;
    }
   case NoteBookFrameID: {
      AFrame = new TEditorsStylesDemoNoteBookFrame(NULL);
      break;
    }
   case IssuesFrameID: {
      AFrame = new TEditorsStylesDemoIssuesFrame(NULL);
      break;
    }
   case StylesPaletteFrameID:
      AFrame = new TEditorsStylesDemoStylesPaletteFrame(NULL);
      break;
  }
  return AFrame;
}
//---------------------------------------------------------------------------

void __fastcall AssignJPEGProperties(TJPEGOptions AJPEGOptions, TJPEGImage* AJPEGImage)
{
  AJPEGImage->CompressionQuality = AJPEGOptions.CompressionQuality;
  AJPEGImage->Grayscale = AJPEGOptions.Grayscale;
  AJPEGImage->ProgressiveEncoding = AJPEGOptions.ProgressiveEncoding;
}
//---------------------------------------------------------------------------

void ConvertBitmapToJPEG(Graphics::TBitmap* ABitmap, String AJPGFileName, TJPEGOptions AJPEGOptions)
{
  TJPEGImage* J = new TJPEGImage();
  try {
    AssignJPEGProperties(AJPEGOptions, J);
    J->Assign(ABitmap);
    J->SaveToFile(AJPGFileName);
  }
  __finally {
    delete J;
  }
}
//---------------------------------------------------------------------------

