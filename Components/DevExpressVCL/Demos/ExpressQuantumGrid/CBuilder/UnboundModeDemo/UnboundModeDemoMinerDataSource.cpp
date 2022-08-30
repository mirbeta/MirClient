#include "UnboundModeDemoMinerDataSource.h"

void __fastcall TMinerFieldDataSource::InitNewGame(void)
{
  FCellState.Length = FColCount;
  for(int i=0; i < FColCount; i++) {
    FCellState[i].Length = FRowCount;
    for(int j=0; j < FRowCount; j++) {
      FCellState[i][j].CellState = csClosed;
      FCellState[i][j].SurroundNumber = 0;
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TMinerFieldDataSource::UpdateMinerFieldState(const TChangedCells AChangedCells, const TCells ARedCells)
{
  for(int i=0; i < AChangedCells.Length; i++) {
    FCellState[AChangedCells[i].Pos.x][AChangedCells[i].Pos.y] = AChangedCells[i].CellState;
 }
}
//---------------------------------------------------------------------------

void __fastcall TMinerFieldDataSource::FireGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TChangedCells &AChangedCells, TCells &ARedCells)
{
  TCells ACells;
  if (FGameStatusChanged != NULL) {
    ACells.Length = AChangedCells.Length;
    for(int i=0; i < AChangedCells.Length; i++)
      ACells[i] = AChangedCells[i].Pos;
    AChangedCells.Length = 0;
    FGameStatusChanged(Sender, AGameStatus, AGameDifficulty, ACells, ARedCells);
  }
}
//---------------------------------------------------------------------------

void __fastcall TMinerFieldDataSource::FireEvMinerFieldChanged(System::TObject* Sender, TCells &AChangedCells, TCells &ARedCells)
{
  if (FMinerFieldChanged != NULL)
    FMinerFieldChanged(Sender, AChangedCells, ARedCells);
}
//---------------------------------------------------------------------------

int __fastcall TMinerFieldDataSource::GetRecordCount(void)
{
  return (FRowCount);
}
//---------------------------------------------------------------------------

Variant __fastcall TMinerFieldDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  if (((int)AItemHandle <= (FColCount - 1)) && ((int)ARecordHandle <= (FRowCount - 1)))
    return ((int)&FCellState[(int)AItemHandle][(int)ARecordHandle]);
  else
    return VarAsType(0, varNull);
}
//---------------------------------------------------------------------------

__fastcall TMinerFieldDataSource::~TMinerFieldDataSource(void)
{
  FCellState.Length = 0;
}
//---------------------------------------------------------------------------

void __fastcall TMinerFieldDataSource::HandleEvMinerFieldChanged(System::TObject* Sender, TChangedCells &AChangedCells, TCells &ARedCells)
{
  UpdateMinerFieldState(AChangedCells, ARedCells);
  DataChanged();
  TCells ACells;
  ACells.Length = AChangedCells.Length;
  for(int i=0; i < AChangedCells.Length; i++)
    ACells[i] = AChangedCells[i].Pos;
  AChangedCells.Length = 0;
  FireEvMinerFieldChanged(Sender, ACells, ARedCells);
}
//---------------------------------------------------------------------------

void __fastcall TMinerFieldDataSource::HandleEvGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TChangedCells &AChangedCells, TCells &ARedCells)
{
  switch(AGameStatus) {
    case gsNew: {
      FColCount = AGameDifficulty.Width;
      FRowCount = AGameDifficulty.Height;
      InitNewGame();
    }; break;
    case gsLost:
      UpdateMinerFieldState(AChangedCells, ARedCells); break;
    case gsWon:
      UpdateMinerFieldState(AChangedCells, ARedCells); break;
  };
  DataChanged();
  FireGameStatusChanged(Sender, AGameStatus, AGameDifficulty, AChangedCells, ARedCells);
}
//---------------------------------------------------------------------------




