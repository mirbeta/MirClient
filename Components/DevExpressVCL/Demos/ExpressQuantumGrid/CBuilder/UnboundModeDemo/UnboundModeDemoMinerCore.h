#include "UnboundModeDemoTypes.h"
#include <SysInit.hpp>
#include <System.hpp>

#ifndef UnboundModeDemoMinerCoreH
#define UnboundModeDemoMinerCoreH

class TMinerField : public TObject {
private:
	int FHeight;
	int FWidth;
	int FMineCount;
	DynamicArray<DynamicArray<TCellStateRec > >  FCellState;
	TGameDifficulty FGameDifficulty;
	TGameStatus FGameStatus;
	int FCellsBombMarkedCount;
	DynamicArray<TPoint >  FRedCells;
	TSrcMinerFieldChangedEvent FMinerFieldChanged;
	TSrcGameStatusChangedEvent FGameStatusChanged;
	bool __fastcall CheckFieldBounds(int AXPos, int AYPos);
	int __fastcall Get_Height(void);
	void __fastcall Set_Height(const int Value);
	int __fastcall Get_Widht(void);
	void __fastcall Set_Width(const int Value);
	int __fastcall Get_MineCount(void);
	void __fastcall Set_MineCount(const int Value);
	TCellStateRec __fastcall Get_CellState(int XIndex, int YIndex);
	void __fastcall Set_CellState(int XIndex, int YIndex, const TCellStateRec &Value);
	TGameDifficulty __fastcall Get_GameDifficulty();
	void __fastcall Set_GameDifficulty(const TGameDifficulty &Value);
	void __fastcall OpenSurround(int AXPos, int AYPos, TCells &AChangedCells);
	TGameStatus __fastcall Get_GameStatus(void);
	void __fastcall Set_GameStatus(const TGameStatus Value);
	void __fastcall CreateNewGame(void);
	bool __fastcall IsGameFinished(void);
	void __fastcall FireEvGameStatusChanged(TGameStatus AGameStatus, TCells &AChangedCells);
	void __fastcall FireEvMinerFieldChanged(TCells &AChangedCells);
	__property TCellStateRec CellState[int XIndex][int YIndex] = {read=Get_CellState, write=Set_CellState};
	__property TGameDifficulty GameDifficulty = {read=Get_GameDifficulty, write=Set_GameDifficulty};
  void __fastcall SetThisDifficulty();
	__property int Height = {read = Get_Height, write = Set_Height};
	__property int Width = {read=Get_Widht, write=Set_Width};
	__property int MineCount = {read=Get_MineCount, write=Set_MineCount};
	__property TGameStatus GameStatus = {read=Get_GameStatus, write=Set_GameStatus};
	void __fastcall FillBombCells(TCells &AChangedCells);
  void __fastcall SetCellState(int AXPos, int AYPos, TCells& AChangedCells);
  bool __fastcall CheckIfClosed_Surround(int AXPos, int AYPos, TCells& AChangedCells);
  void __fastcall SetSurround(int AXPos, int AYPos);
  bool __fastcall IsCountBombMarkedCorrect(int AXPos, int AYPos, bool &WrongBombMark);
protected:
	virtual void __fastcall DropMines(int AFirstX, int AFirstY);
	virtual void __fastcall OpenCell(int AXPos, int AYPos);
	virtual void __fastcall BombMarkCell(int AXPos, int AYPos);
	virtual void __fastcall QuestionMarkCell(int AXPos, int AYPos);
	virtual void __fastcall CloseCell(int AXPos, int AYPos);
	virtual void __fastcall CheckSurround(int AXPos, int AYPos);

public:
	__property TSrcMinerFieldChangedEvent OnMinerFieldChanged = {read=FMinerFieldChanged, write=FMinerFieldChanged};
	__property TSrcGameStatusChangedEvent OnGameStatusChanged = {read=FGameStatusChanged, write=FGameStatusChanged};
	/* TObject.Create */ inline __fastcall TMinerField(void) : System::TObject() { FGameStatus = gsNew; }
//	__fastcall TMinerField(void) {FGameStatus = gsNew;};
	__fastcall ~TMinerField(void);
	void __fastcall HandleEvCreateNewGame(System::TObject* Sender);
	void __fastcall HandleEvChangeGameDifficulty(System::TObject* Sender, const TGameDifficulty &AGameDifficulty);
	void __fastcall HandleMinerFieldActionEvent(System::TObject* Sender, int ACol, int ARow, TMinerFieldActionEventType AMinerFieldEventType);
};

#endif
