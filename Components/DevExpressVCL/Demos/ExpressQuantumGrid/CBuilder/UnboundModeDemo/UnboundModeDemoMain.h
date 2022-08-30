//---------------------------------------------------------------------------

#ifndef UnboundModeDemoMainH
#define UnboundModeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ExtCtrls.hpp>
#include "UnboundModeDemoIntMinerField.h"
#include "cxLookAndFeels.hpp"
//---------------------------------------------------------------------------
class TUnboundModeDemoMainForm : public TForm
{
__published:  // IDE-managed Components
  TMainMenu *mmMain;
  TMenuItem *miGame;
  TMenuItem *miNew;
  TMenuItem *sep1;
  TMenuItem *miBeginner;
  TMenuItem *miIntermediate;
  TMenuItem *miExpert;
  TMenuItem *miCustom;
  TMenuItem *sep2;
  TMenuItem *miMarks;
  TMenuItem *miColors;
  TMenuItem *miBlue;
  TMenuItem *miGold;
  TMenuItem *miGreen;
  TMenuItem *miSystem;
  TMenuItem *sep3;
  TMenuItem *miBestTimes;
  TMenuItem *N1;
  TMenuItem *miExit;
  TMenuItem *miAbout;
  TImageList *ilGame;
  TImageList *ilNumbers;
  TTimer *Timer;
  TImageList *ilFaces;
  TcxLookAndFeelController *LookAndFeelController;
  void __fastcall miAboutClick(TObject *Sender);
  void __fastcall miExitClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall FormMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
  void __fastcall FormMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall FormPaint(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall miNewClick(TObject *Sender);
  void __fastcall miBeginnerClick(TObject *Sender);
  void __fastcall miIntermediateClick(TObject *Sender);
  void __fastcall miExpertClick(TObject *Sender);
  void __fastcall miCustomClick(TObject *Sender);
  void __fastcall miBestTimesClick(TObject *Sender);
  void __fastcall miMarksClick(TObject *Sender);
  void __fastcall miBlueClick(TObject *Sender);
  void __fastcall miGreenClick(TObject *Sender);
  void __fastcall miSystemClick(TObject *Sender);
  void __fastcall miGoldClick(TObject *Sender);
  void __fastcall TimerTimer(TObject *Sender);
private:  // User declarations
    String FNames[3];
    int FTimes[3];
    TGameDifficulty FGameDifficulty;
    int FImageIndex;
    bool FDown;
    int FMineCount;
    int FTime;
    TChangeGameDifficultyEvent FOnChangeGameDifficulty;
    bool FMouseButtonPressed;
    TIntMinerField* IntMinerField;
    TMinerField* MinerField;
    bool __fastcall IsPointInRect(TPoint APoint, TRect ARect);
    void __fastcall SetButtonBounds(TRect &ARect);
    void __fastcall FireGameDifficultyChangedEvent(const TGameDifficulty ANewGameDifficulty);
    void __fastcall DrawMineCount();
    void __fastcall DrawTime();
    void __fastcall DrawButton();
    void __fastcall DrawIndicatorBoard();
    void __fastcall DrawOuterFrame();
    void __fastcall ReadMinerSettings();
    void __fastcall WriteMinerSettings();
    void __fastcall InitGameSettings();
    void __fastcall ResetFastestTimes();
    void __fastcall CheckBestTimes();
    void __fastcall CheckMenuItem(TDifficultyType AGameDifficulty);
public:   // User declarations
  __fastcall TUnboundModeDemoMainForm(TComponent* Owner);
    void __fastcall HandleMineCountChangedEvent(TObject* Sender, TMineCountChangedEventType AMineCountChangedEventType);
    void __fastcall HandleEvGameStatusChanged(TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty);
    void __fastcall HandleEvImageChanged(TObject* Sender, int AImageIndex);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundModeDemoMainForm *UnboundModeDemoMainForm;
//---------------------------------------------------------------------------
#endif
