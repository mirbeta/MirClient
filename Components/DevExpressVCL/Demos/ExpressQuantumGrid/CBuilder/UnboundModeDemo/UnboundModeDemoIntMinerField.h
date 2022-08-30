#include <cxControls.hpp>
#include <cxGridCustomView.hpp>
#include <cxGridTableView.hpp>
#include "UnboundModeDemoMinerDataSource.h"
#include "UnboundModeDemoTypes.h"
#include <cxGraphics.hpp>
#include <cxGridCustomTableView.hpp>
#include "UnboundModeDemoMinerCore.h"
#include <cxGrid.hpp>
#include <StdCtrls.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>


#ifndef UnboundModeDemoIntMinerFieldH
#define UnboundModeDemoIntMinerFieldH

class TControlAccess: public TControl {
public:
	DYNAMIC void __fastcall Resize(void);
};

class TIntMinerField: public TObject {
private:
  TMinerField* FMinerField;
  TcxCustomGrid* FGrid;
  int FColCount;
  int FRowCount;
  int FCellWidth;
  TGameStatus FGameStatus;
  DynamicArray<TPoint >  FRedCells;
  DynamicArray<Graphics::TColor >  FSurroundColors;
  TMinerFieldDataSource* FMinerFieldDataSource;
  TColor FOpenCellBkColor;
  TColor FClosedCellBkColor;
  TColor FFrameColor;
  TColor FRectangleColor;
  bool FQuestionMarkCell;
  Controls::TImageList* FImages;
  DynamicArray<TPoint>  FPressedCells;
  TGameDifficulty FGameDifficulty;
  bool FSurprised;
  Classes::TNotifyEvent FCreateNewGameEvent;
  TMinerFieldActionEvent FOnMinerFieldAction;
  TMineCountChangedEvent FMineCountChanged;
  TImageChangedEvent FOnImageChanged;
  TFormGameStatusChangedEvent FGameStatusChanged;
  UnboundModeDemoTypesH::TColorScheme FColorScheme;
  void __fastcall SetSchemeColors(void);
  TCellStateRec __fastcall GetCellState(int ACol, int ARow);
  void __fastcall AddPressedCell(int ACol, int ARow);
  bool __fastcall CheckFieldBounds(int AXPos, int AYPos);
  void __fastcall InitNewGame(void);
  void __fastcall UnpressAndInvalidate(void);
  void __fastcall InvalidateCells(const TCells AChangedCells, const TCells ARedCells);
  void __fastcall UpdateMinerFieldState(const TCells ARedCells);
  void __fastcall DrawCell(const TCellStateRec ACellState, int ACol, int ARow, const TRect &ARect, Graphics::TCanvas* ACanvas);
  void __fastcall InvalidateCell(int ACol, int ARow);
  void __fastcall MouseDownHandler(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
  void __fastcall MouseMoveHandler(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y);
  void __fastcall MouseUpHandler(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
  void __fastcall HandleEvMinerFieldChanged(System::TObject* Sender, TCells &AChangedCells, TCells &ARedCells);
  void __fastcall HandleEvGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TCells &AChangedCells, TCells &ARedCells);
  __property TCellStateRec CellState[int ACol][int ARow] = {read=GetCellState};
  void __fastcall SetColorScheme(const UnboundModeDemoTypesH::TColorScheme Value);
  void __fastcall SetNumberColors(void);
  void __fastcall DrawOpenedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState);
  void __fastcall DrawClosedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState, int ACol, int ARow);
  void __fastcall DrawBombMarkedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState);
  void __fastcall DrawQuestionMarkedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState, int ACol, int ARow);
  void __fastcall SetGridViewOptions(TcxGridTableView* GridView);
protected:
  virtual void __fastcall FireMinerFieldEvent(int ACol, int ARow, TMinerFieldActionEventType AMinerFieldActionEventType);
  virtual void __fastcall FireNewGameEvent(void);
  virtual void __fastcall FireImageChanged(int AImageIndex);
  virtual void __fastcall FireSetMineCountEvent(TMineCountChangedEventType AMineCountChangedEventType);
  virtual void __fastcall FireGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty);
  virtual void __fastcall MouseToCell(int X, int Y, int &ACol, int &ARow);
  void __fastcall CustomDrawCellHandler(Cxgridcustomtableview::TcxCustomGridTableView* Sender, Cxgraphics::TcxCanvas* ACanvas, Cxgridcustomtableview::TcxGridTableDataCellViewInfo* AViewInfo, bool &ADone);
public:
  __fastcall TIntMinerField(Classes::TComponent* AOwner, TMinerField* AMinerField);
  __fastcall ~TIntMinerField(void);
	void __fastcall CreateNewGame(void);
	void __fastcall SetParent(TWinControl* AParent);
  __property TMinerField* MinerField = {read=FMinerField, write=FMinerField};
  __property bool QuestionMarkCell = {read=FQuestionMarkCell, write=FQuestionMarkCell};
  __property Controls::TImageList* Images = {read=FImages, write=FImages};
  __property UnboundModeDemoTypesH::TColorScheme ColorScheme = {read=FColorScheme, write=SetColorScheme};
  __property TMinerFieldActionEvent OnMinerFieldAction = {read=FOnMinerFieldAction, write=FOnMinerFieldAction};
  __property TImageChangedEvent OnImageChanged = {read=FOnImageChanged, write=FOnImageChanged};
  __property TMineCountChangedEvent OnMineCountChanged = {read=FMineCountChanged, write=FMineCountChanged};
  __property TFormGameStatusChangedEvent OnGameStatusChanged = {read=FGameStatusChanged, write=FGameStatusChanged};
};

#endif
