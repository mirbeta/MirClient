#include "UnboundModeDemoTypes.h"
#include <cxCustomData.hpp>
#include <SysInit.hpp>
#include <System.hpp>

#ifndef UnboundModeDemoMinerDataSourceH
#define UnboundModeDemoMinerDataSourceH

class TMinerFieldDataSource : public Cxcustomdata::TcxCustomDataSource {
private:
	DynamicArray<DynamicArray<TCellStateRec > >  FCellState;
	int FColCount;
	int FRowCount;
	TIntGameStatusChangedEvent FGameStatusChanged;
	TIntMinerFieldChangedEvent FMinerFieldChanged;
	void __fastcall InitNewGame(void);
	void __fastcall UpdateMinerFieldState(const TChangedCells AChangedCells, const TCells ARedCells);
	void __fastcall FireGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, /*Unboundmodedemo*/TChangedCells &AChangedCells, /*Unboundmodedemo*/TCells &ARedCells);
	void __fastcall FireEvMinerFieldChanged(System::TObject* Sender, TCells &AChangedCells, TCells &ARedCells);

protected:
	int __fastcall GetRecordCount(void);
	Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);

public:
	__fastcall ~TMinerFieldDataSource(void);
	void __fastcall HandleEvMinerFieldChanged(System::TObject* Sender, TChangedCells &AChangedCells, TCells &ARedCells);
	void __fastcall HandleEvGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, /*Unboundmodedemo*/TChangedCells &AChangedCells, /*Unboundmodedemo*/TCells &ARedCells);
	__property TIntMinerFieldChangedEvent OnMinerFieldChanged = {read=FMinerFieldChanged, write=FMinerFieldChanged};
	__property TIntGameStatusChangedEvent OnGameStatusChanged = {read=FGameStatusChanged, write=FGameStatusChanged};
};

#endif
