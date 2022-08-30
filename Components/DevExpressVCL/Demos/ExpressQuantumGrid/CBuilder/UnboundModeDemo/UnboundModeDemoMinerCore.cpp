#include "UnboundModeDemoMinerCore.h"

bool __fastcall TMinerField::CheckFieldBounds(int AXPos, int AYPos)
{
   bool Result = false;
   if ((AXPos >=0) && (AYPos >=0) && (AXPos < FCellState.Length) &&
   (AYPos < FCellState[0].Length)) Result = true;
   return (Result);
}
//---------------------------------------------------------------------------

int __fastcall TMinerField::Get_Height(void)
{
  return(FHeight);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_Height(const int Value)
{
  if((9 <= Value) && (Value <= 24))
    FHeight = Value; else
  if(Value < 9)
    FHeight = 9; else
  if(24 < Value)
    FHeight = 24;
}
//---------------------------------------------------------------------------

int __fastcall TMinerField::Get_Widht(void)
{
  return(FWidth);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_Width(const int Value)
{
  if((9 <= Value) && (Value <= 30))
    FWidth = Value; else
  if((Value < 9))
    FWidth = 9; else
  if(30 < Value)
    FWidth = 30;
}
//---------------------------------------------------------------------------

int __fastcall TMinerField::Get_MineCount(void)
{
  return (FMineCount);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_MineCount(const int Value)
{
  if((10 <= Value) && (Value <= (FHeight - 1)*(FWidth - 1)))
    FMineCount = Value; else
  if(Value < 10)
    FMineCount = 10; else
  if((FHeight - 1) * (FWidth - 1) < Value)
    FMineCount = (FHeight - 1)*(FWidth - 1);
}
//---------------------------------------------------------------------------

TCellStateRec __fastcall TMinerField::Get_CellState(int XIndex, int YIndex)
{
  return(FCellState[XIndex][YIndex]);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_CellState(int XIndex, int YIndex, const TCellStateRec &Value)
{
  if((FCellState[XIndex][YIndex].SurroundNumber != Value.SurroundNumber) ||
    (FCellState[XIndex][YIndex].CellState != Value.CellState)) {
      if(FCellState[XIndex][YIndex].CellState == csBombMarked)
        FCellsBombMarkedCount--;

      FCellState[XIndex][YIndex] = Value;
      if(Value.CellState == csBombMarked)
        FCellsBombMarkedCount++;
    };
}
//---------------------------------------------------------------------------

TGameDifficulty __fastcall TMinerField::Get_GameDifficulty()
{
  return(FGameDifficulty);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::SetThisDifficulty()
{
  FGameDifficulty.Height = Height;
  FGameDifficulty.Width = Width;
  FGameDifficulty.MineCount = MineCount;
};
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_GameDifficulty(const TGameDifficulty &Value)
{
  TCells ACells;
  FGameDifficulty.DifficultyType = Value.DifficultyType;
  switch(Value.DifficultyType) {
    case dtBeginner: {
      Height = 9;
      Width = 9;
      MineCount = 10;
    }; break;
    case dtIntermediate: {
      Height = 16;
      Width = 16;
      MineCount = 40;
    }; break;
    case dtExpert: {
      Height = 16;
      Width = 30;
      MineCount = 99;
    }; break;
    case dtCustom: {
      Height = Value.Height;
      Width = Value.Width;
      MineCount = Value.MineCount;
    }; break;
  };
  SetThisDifficulty();
  GameStatus = gsNew;
  FireEvGameStatusChanged(gsNew, ACells);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::SetCellState(int AXPos, int AYPos, TCells& AChangedCells)
{
  if (FCellState[AXPos][AYPos].SurroundNumber != -1)
    FCellState[AXPos][AYPos].CellState = csOpened;
  AChangedCells.Length = AChangedCells.Length + 1;
  AChangedCells[AChangedCells.Length - 1].x = AXPos;
  AChangedCells[AChangedCells.Length - 1].y = AYPos;
}
//---------------------------------------------------------------------------

bool __fastcall TMinerField::CheckIfClosed_Surround(int AXPos, int AYPos, TCells& AChangedCells)
{
  bool Result = false;
  if ((FCellState[AXPos][AYPos].CellState == csClosed)
    || (FCellState[AXPos][AYPos].CellState == csQuestionMarked))
    if (FCellState[AXPos][AYPos].SurroundNumber == 0)
      Result = true;
    else
      SetCellState(AXPos, AYPos, AChangedCells);
  return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::OpenSurround(int AXPos, int AYPos, TCells &AChangedCells)
{
  SetCellState(AXPos, AYPos, AChangedCells);
  for (int i=-1; i <= 1; i++)
    for (int j=-1; j <= 1; j++)
      if(CheckFieldBounds(AXPos+i, AYPos+j) && CheckIfClosed_Surround(AXPos+i, AYPos+j, AChangedCells))
        OpenSurround(AXPos+i, AYPos+j, AChangedCells);
}
//---------------------------------------------------------------------------

TGameStatus __fastcall TMinerField::Get_GameStatus(void)
{
  return(FGameStatus);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::Set_GameStatus(const TGameStatus Value)
{
  FGameStatus = Value;
  if(FGameStatus == gsNew)
    CreateNewGame();
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::CreateNewGame(void)
{
  FCellsBombMarkedCount = 0;
  FRedCells.Length = 0;
  FCellState.Length = 0;
  FCellState.Length = FWidth;
  for(int i=0; i < FWidth; i++) {
    FCellState[i].Length = FHeight;
    for(int j=0; j < FHeight; j++) {
      FCellState[i][j].CellState = csClosed;
      FCellState[i][j].SurroundNumber = 0;
    }
  };
}
//---------------------------------------------------------------------------

bool __fastcall TMinerField::IsGameFinished(void)
{
  bool Result = false;
  int FreeCells = 0;
  for(int i=0; i < FCellState.Length; i++)
    for(int j=0; j < FCellState[i].Length; j++)
      if((FCellState[i][j].CellState == csClosed) ||
        (FCellState[i][j].CellState == csQuestionMarked)) {
        FreeCells++;
        if(FreeCells > FMineCount) return (Result);
      };
  if(FreeCells == (FMineCount - FCellsBombMarkedCount)) Result = true;
  return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::FireEvGameStatusChanged(TGameStatus AGameStatus, TCells &AChangedCells)
{
  TChangedCells ACells;
  TCells ARedCells;
  if(FGameStatusChanged != NULL) {
    ACells.Length = AChangedCells.Length;
    for(int i=0; i < AChangedCells.Length; i++) {
      ACells[i].Pos = AChangedCells[i];
      ACells[i].CellState = FCellState[AChangedCells[i].x][AChangedCells[i].y];
    };
    ARedCells.Length = FRedCells.Length;
    for(int i=0; i < FRedCells.Length; i++)
      ARedCells[i] = FRedCells[i];
    FGameStatusChanged(this, AGameStatus, FGameDifficulty, ACells, ARedCells);
  };
  AChangedCells.Length = 0;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::FireEvMinerFieldChanged(TCells &AChangedCells)
{
  TChangedCells ACells;
  TCells ARedCells;
  if(FMinerFieldChanged != NULL) {
    ACells.Length = AChangedCells.Length;
    for(int i=0; i < AChangedCells.Length; i++) {
      ACells[i].Pos.x = AChangedCells[i].x;
      ACells[i].Pos.y = AChangedCells[i].y;
      ACells[i].CellState.CellState = FCellState[AChangedCells[i].x][AChangedCells[i].y].CellState;
      ACells[i].CellState.SurroundNumber = FCellState[AChangedCells[i].x][AChangedCells[i].y].SurroundNumber;
    };
    AChangedCells.Length = 0;
    ARedCells.Length = FRedCells.Length;
    for(int i=0; i < FRedCells.Length; i++)
      ARedCells[i] = FRedCells[i];

    FMinerFieldChanged(this, ACells, ARedCells);
  };
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::FillBombCells(TCells &AChangedCells)
{
  for(int i=0; i < FCellState.Length; i++)
    for(int j=0; j < FCellState[i].Length; j++)
      if(FCellState[i][j].SurroundNumber == -1) {
        AChangedCells.Length++;
        AChangedCells[AChangedCells.Length - 1] = Point(i, j);
      };
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::SetSurround(int AXPos, int AYPos)
{
  for(int i=-1; i <=1; i++)
    for(int j=-1; j <=1; j++)
      if(CheckFieldBounds(AXPos+i, AYPos+j) && (FCellState[AXPos+i][AYPos+j].SurroundNumber != -1))
        FCellState[AXPos+i][AYPos+j].SurroundNumber++;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::DropMines(int AFirstX, int AFirstY)
{
  int XPos, YPos, DroppedNumber;
  int DeadNumber = FWidth * AFirstY + AFirstX;
  int RandomBase = FWidth * FHeight;
  Randomize();
  int DroppedMines = 0;
  while(DroppedMines < FMineCount) {
    DroppedNumber = random(RandomBase);

    XPos = div(DroppedNumber, FWidth).rem;
    YPos = div(DroppedNumber, FWidth).quot;

    if((FCellState[XPos][YPos].SurroundNumber != -1) && (DroppedNumber != DeadNumber)) {
      FCellState[XPos][YPos].SurroundNumber = -1;
      SetSurround(XPos, YPos);
      DroppedMines++;
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::OpenCell(int AXPos, int AYPos)
{
  TCells ChangedCells;
  if(GameStatus == gsNew) {
    DropMines(AXPos, AYPos);
    FGameStatus = gsRun;
    FireEvGameStatusChanged(gsRun, ChangedCells);
  };

  if(FCellState[AXPos][AYPos].CellState == csBombMarked)
    return;
  if(FCellState[AXPos][AYPos].SurroundNumber == -1) {
    // Stop current game
    FGameStatus = gsLost;
    FRedCells.Length = FRedCells.Length + 1;
    FRedCells[0].x = AXPos; FRedCells[0].y = AYPos;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsLost, ChangedCells);
    return;
  };

  if(FCellState[AXPos][AYPos].SurroundNumber != 0) {
    FCellState[AXPos][AYPos].CellState = csOpened;
    ChangedCells.Length = 1;
    ChangedCells[0].x = AXPos; ChangedCells[0].y = AYPos;
  }
  else
    OpenSurround(AXPos, AYPos, ChangedCells);
  FireEvMinerFieldChanged(ChangedCells);

  if(IsGameFinished()) {
    FGameStatus = gsWon;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsWon, ChangedCells);
  };
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::BombMarkCell(int AXPos, int AYPos)
{
  TCellStateRec cState;
  cState.SurroundNumber = CellState[AXPos][AYPos].SurroundNumber;
  cState.CellState = csBombMarked;
  CellState[AXPos][AYPos] = cState;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::QuestionMarkCell(int AXPos, int AYPos)
{
  TCellStateRec cState;
  cState.SurroundNumber = CellState[AXPos][AYPos].SurroundNumber;
  cState.CellState = csQuestionMarked;
  CellState[AXPos][AYPos] = cState;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::CloseCell(int AXPos, int AYPos)
{
  TCellStateRec cState;
  cState.SurroundNumber = CellState[AXPos][AYPos].SurroundNumber;
  cState.CellState = csClosed;
  CellState[AXPos][AYPos] = cState;
}
//---------------------------------------------------------------------------

bool __fastcall TMinerField::IsCountBombMarkedCorrect(int AXPos, int AYPos, bool &WrongBombMark)
{
  int BombMarked = 0;
  int RealBombCount = 0;
  for(int i=-1; i <= 1; i++)
    for(int j=-1; j<=1; j++)
      if(CheckFieldBounds(AXPos+i, AYPos+j)) {
        if(FCellState[AXPos+i][AYPos+j].CellState == csBombMarked) {
          BombMarked++;
          if(FCellState[AXPos+i][AYPos+j].SurroundNumber != -1)
            WrongBombMark = true;
        };
        if(FCellState[AXPos + i][AYPos + j].SurroundNumber == -1) {
          FRedCells.Length = FRedCells.Length + 1;
          FRedCells[FRedCells.Length - 1].x = AXPos+i;
          FRedCells[FRedCells.Length - 1].y = AYPos+j;
          RealBombCount++;
        };
      };
  return(RealBombCount == BombMarked);
};

void __fastcall TMinerField::CheckSurround(int AXPos, int AYPos)
{
  bool WrongBombMark;
  TCells ChangedCells;
  if((FCellState[AXPos][AYPos].CellState == csBombMarked) ||
    (FCellState[AXPos][AYPos].CellState == csClosed) ||
    (FCellState[AXPos][AYPos].CellState == csQuestionMarked))
    return;

  // check whether csBombMarked Cells set well
  WrongBombMark = false;
  if(!IsCountBombMarkedCorrect(AXPos, AYPos, WrongBombMark)) {
    FRedCells.Length = 0;
    return;
  };

  // open surrounding csClosed Cells
  OpenSurround(AXPos, AYPos, ChangedCells);

  if(WrongBombMark) {
    FGameStatus = gsLost;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsLost, ChangedCells);
    return;
  };
  FRedCells.Length = 0;

  FireEvMinerFieldChanged(ChangedCells);
  if(IsGameFinished()) {
    FGameStatus = gsWon;
    FillBombCells(ChangedCells);
    FireEvGameStatusChanged(gsWon, ChangedCells);
  };
}
//---------------------------------------------------------------------------

__fastcall TMinerField::~TMinerField(void)
{
  FRedCells.Length = 0;
  FCellState.Length = 0;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::HandleEvCreateNewGame(System::TObject* Sender)
{
  GameStatus = gsNew;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::HandleEvChangeGameDifficulty(System::TObject* Sender, const TGameDifficulty &AGameDifficulty)
{
  GameDifficulty = AGameDifficulty;
}
//---------------------------------------------------------------------------

void __fastcall TMinerField::HandleMinerFieldActionEvent(System::TObject* Sender, int ACol, int ARow, TMinerFieldActionEventType AMinerFieldEventType)
{
  TCells AChangedCell;
  AChangedCell.Length = 1;
  AChangedCell[AChangedCell.Length - 1].x = ACol;
  AChangedCell[AChangedCell.Length - 1].y = ARow;
  switch(AMinerFieldEventType) {
    case meOpenCell: OpenCell(ACol, ARow); break;
    case meCloseCell: {
      CloseCell(ACol, ARow);
      FireEvMinerFieldChanged(AChangedCell);
    }; break;
    case meBombMarkCell: {
      BombMarkCell(ACol ,ARow);
      FireEvMinerFieldChanged(AChangedCell);
    }; break;
    case meQuestionMarkCell: {
      QuestionMarkCell(ACol, ARow);
      FireEvMinerFieldChanged(AChangedCell);
    }; break;
    case meCheckSurround: CheckSurround(ACol, ARow); break;
  };
}
//---------------------------------------------------------------------------
