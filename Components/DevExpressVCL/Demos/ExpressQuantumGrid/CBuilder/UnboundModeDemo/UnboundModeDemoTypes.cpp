#include "UnboundModeDemoTypes.h"
const char* Difficulty = "Difficulty";
const char* Width = "Width";
const char* Height = "Height";
const char* MineCount = "MineCount";
const char* Mark = "Mark";
const char* Name1 = "Name1";
const char* Name2 = "Name2";
const char* Name3 = "Name3";
const char* Time1 = "Time1";
const char* Time2 = "Time2";
const char* Time3 = "Time3";
const char* Section = "Software\\Developer Express\\UnboundModeDemo";
const TSchemeColors SchemeColors = {{clBlueBright, clBlueLight, clWhite, clBlueDark, clBlueSky, clBlueSky,   clBlueDark, clWhite, clBlueLight},
                                 {clGold, clGreyLight, clWhite, clGoldDark, clGreyDark, clGreyDark, clGoldDark, clWhite, clGreyLight},
                                 {clGreenBright, clGreenLight, clWhite, clGreenDark, clGreenObscured, clGreenLight, clGreenDark, clWhite, clGreenLight},
                                 {clSilverDark, clSilver, clWhite, clGray, clSilver, clSilver, clGray, clWhite, clSilver}};

void __fastcall MakeArrayFromInt(int AInt, TArrInteger &AArrInt, int MinArrCount)
{
  AArrInt.Length = MinArrCount;
  for (int i=0; i < MinArrCount; i++)
    AArrInt[i] = 0;
  int Ind = 0;
  while (AInt != 0) {
    if (Ind >= MinArrCount)
      AArrInt.Length = AArrInt.Length + 1;
    AArrInt[Ind] = div(AInt, 10).rem;
    AInt = div(AInt, 10).quot;
    Ind++;
  }
}
//---------------------------------------------------------------------------

bool __fastcall IsExistsInArray(TCells AArr, int ACol, int ARow)
{
  for (int i=0; i < AArr.Length; i++)
    if ((AArr[i].x == ACol) && (AArr[i].y == ARow))
      return (true);
 return (false);
}
//---------------------------------------------------------------------------

void __fastcall SetFormPosition(Forms::TForm* AForm, int AXPos, int AYPos)
{
  PRect pWorkArea;
  pWorkArea = new TRect;
  SystemParametersInfo(SPI_GETWORKAREA, 0,  pWorkArea, 0);

  TRect WorkArea = *pWorkArea;

  delete pWorkArea;

  if (((TForm*)AForm->Owner)->Left + AForm->Width + AXPos > WorkArea.Right)
    AForm->Left = WorkArea.Right - AForm->Width; else
  if (((TForm*)AForm->Owner)->Left + AXPos < WorkArea.Left)
    AForm->Left = WorkArea.Left; else
  AForm->Left = ((TForm*)AForm->Owner)->Left + AXPos;

  if (((TForm*)AForm->Owner)->Top + AForm->Height + AYPos > WorkArea.Bottom)
    AForm->Top = WorkArea.Bottom - AForm->Height - 4; else
  AForm->Top = ((TForm*)AForm->Owner)->Top + AYPos;
}
//---------------------------------------------------------------------------


