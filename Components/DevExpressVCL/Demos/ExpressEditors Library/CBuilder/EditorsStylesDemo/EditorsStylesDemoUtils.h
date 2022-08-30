//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoUtilsH
#define EditorsStylesDemoUtilsH
#include "EditorsStylesDemoBase.h"
#include "Graphics.hpp"

struct TJPEGOptions {
  Shortint CompressionQuality;
  bool Grayscale;
  bool ProgressiveEncoding;
};

const int TextProcessingFrameID = 0;
const int ImageProcessingFrameID = 1;
const int SolarSystemFrameID = 2;
const int NoteBookFrameID = 3;
const int IssuesFrameID = 4;
const int StylesPaletteFrameID = 5;

TEditorsStylesDemoBaseFrame* __fastcall CreateFrameByID(int AID);
void ConvertBitmapToJPEG(Graphics::TBitmap* ABitmap, String AJPGFileName, TJPEGOptions AJPEGOptions);

#endif

