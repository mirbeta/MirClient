#include "UnboundModeDemoIntMinerField.h"

void __fastcall TIntMinerField::SetSchemeColors(void)
{
  FOpenCellBkColor = SchemeColors[(int)FColorScheme][0];
  FClosedCellBkColor = SchemeColors[(int)FColorScheme][1];
  FFrameColor = SchemeColors[(int)FColorScheme][2];
  FRectangleColor = SchemeColors[(int)FColorScheme][3];
}
//---------------------------------------------------------------------------

TCellStateRec __fastcall TIntMinerField::GetCellState(int ACol, int ARow)
{
  return (*(PCellStateRec)(int)((TcxGridTableView*)FGrid->Views[0])->
    ViewInfo->RecordsViewInfo->Items[ARow]->GetCellViewInfoByItem(
    ((TcxGridTableView*)FGrid->Views[0])->Items[ACol])->Value );
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::AddPressedCell(int ACol, int ARow)
{
  FPressedCells.Length++;
  FPressedCells[FPressedCells.Length - 1].x = ACol;
  FPressedCells[FPressedCells.Length - 1].y = ARow;
}
//---------------------------------------------------------------------------

bool __fastcall TIntMinerField::CheckFieldBounds(int AXPos, int AYPos)
{
   bool Result = false;
   if((AXPos >=0) && (AYPos >=0) && (AXPos < FColCount) &&
     (AYPos < FRowCount)) Result = true;
   return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::InitNewGame(void)
{
  FRedCells.Length = 0;

  FColCount = FGameDifficulty.Width;
  FRowCount = FGameDifficulty.Height;
  FGrid->BeginUpdate();
  try {

	FGrid->Width = FColCount * FCellWidth + 2;
    FGrid->Height = FRowCount * FCellWidth + 2;
	FGrid->Top = psBorder + biBoardHeight - psOuterFrameWidth;
    FGrid->Left = psBorder;

	int par = ((TForm*)FGrid->Owner)->ClientRect.Right - ((TForm*)FGrid->Owner)->ClientRect.Left;
    par = ((TForm*)FGrid->Owner)->Width - par - psOuterFrameWidth;
	((TForm*)FGrid->Owner)->Width = 2 * psBorder + FGrid->Width + par;

	par = ((TForm*)FGrid->Owner)->ClientRect.Bottom - ((TForm*)FGrid->Owner)->ClientRect.Top;
	par = ((TForm*)FGrid->Owner)->Height - par;
	((TForm*)FGrid->Owner)->Height = par +  2 * (psBorder - psOuterFrameWidth) + biBoardHeight + FGrid->Height;
	((TControlAccess*)FGrid->Owner)->Resize();

    if (!(FGrid->Enabled)) FGrid->Enabled = true;
    TcxGridColumn* GridColumn;
    for(int i = ((TcxGridTableView*)FGrid->Views[0])->ColumnCount - 1; i == FColCount; i--)
      delete ((TcxGridTableView*)FGrid->Views[0])->Columns[i];
    for(int i=0; i < FGameDifficulty.Width; i++) {
	  if (i >= ((TcxGridTableView*)FGrid->Views[0])->ColumnCount) {
        GridColumn = ((TcxGridTableView*)FGrid->Views[0])->CreateColumn();
        GridColumn->MinWidth = FCellWidth;
        GridColumn->Width = FCellWidth;
      }
    }
  }
  __finally {
    FGrid->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::UnpressAndInvalidate(void)
{
  TCells CellsToInvalidate;
  CellsToInvalidate.Length = FPressedCells.Length;
  for(int i=0; i < FPressedCells.Length; i++)
    CellsToInvalidate[i] = FPressedCells[i];
  FPressedCells.Length = 0;
  for(int i=0; i < CellsToInvalidate.Length; i++)
    InvalidateCell(CellsToInvalidate[i].x, CellsToInvalidate[i].y);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::InvalidateCells(const TCells AChangedCells, const TCells ARedCells)
{
  for(int i=0; i < AChangedCells.Length; i++)
    InvalidateCell(AChangedCells[i].x, AChangedCells[i].y);
  for(int i=0; i < ARedCells.Length; i++)
    InvalidateCell(ARedCells[i].x, ARedCells[i].y);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::UpdateMinerFieldState(const TCells ARedCells)
{
  FRedCells.Length = ARedCells.Length;
  for(int i=0; i < ARedCells.Length; i++)
    FRedCells[i] = ARedCells[i];
}
//---------------------------------------------------------------------------

void AlignTextInCell(TCanvas* ACanvas, TRect Rect, String AStr, TAlignment Alignment = taCenter)
{
  int Y = 3;
  int X = 1;
  switch(Alignment) {
   case taCenter: X = ((Rect.Right - Rect.Left) - ACanvas->TextWidth(AStr)) >> 1; break;
   case taLeftJustify: X = 1; break;
   case taRightJustify:  X = ((Rect.Right - Rect.Left) - ACanvas->TextWidth(AStr)) -1; break;
  }
  ACanvas->TextOut(Rect.Left + X, Rect.Top + Y, AStr);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::DrawOpenedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState)
{
  ACanvas->Brush->Color = FOpenCellBkColor;
  ACanvas->FillRect(ARect);
  ACanvas->Pen->Style = psSolid;
  ACanvas->Pen->Color = FRectangleColor;
  ARect.Left--; ARect.Top--;
  ACanvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  ARect.Right--; ARect.Bottom--;
  ACanvas->Font->Style = TFontStyles() << fsBold;
  if(ACellState.SurroundNumber >= 1)
    ACanvas->Font->Color = FSurroundColors[ACellState.SurroundNumber-1];
  FGrid->Brush->Style = bsClear;
  if(ACellState.SurroundNumber != 0)
    AlignTextInCell(ACanvas, ARect, IntToStr(ACellState.SurroundNumber));
}

void __fastcall TIntMinerField::DrawClosedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState, int ACol, int ARow)
{
  if(FGameStatus == gsLost) {
    if(IsExistsInArray(FRedCells, ACol, ARow)) {
      // red bomb on an empty background
      ACanvas->Brush->Color = FOpenCellBkColor;
      ACanvas->FillRect(ARect);

      ACanvas->Brush->Style = bsSolid;
      ACanvas->Brush->Color = clRed;
      ARect.Right--;
      ARect.Bottom--;
      ACanvas->FillRect(ARect);
      ACanvas->Brush->Style = bsClear;
      ARect.Left++; ARect.Top++;
      ARect.Right++; ARect.Bottom--;
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imRedBomb, true);
      return;
    }
    if(ACellState.SurroundNumber == -1) {
      ACanvas->Brush->Color = FOpenCellBkColor;
      ACanvas->FillRect(ARect);
      ACanvas->Pen->Style = psSolid;
      ACanvas->Pen->Color = FRectangleColor;
      ARect.Left--; ARect.Top--;
      ACanvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      ARect.Left = ARect.Left + 2;
      ARect.Top = ARect.Top + 2;
      ACanvas->Brush->Style = bsClear;
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBomb, true);  // bomb image
    }
    else
      Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
  }
  else
  if((FGameStatus == gsNew) || (FGameStatus == gsRun)) {
    if(IsExistsInArray(FPressedCells, ACol, ARow))
      Frame3D(ACanvas, ARect, FOpenCellBkColor, FFrameColor, 1); // pressed
    else 
      Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
  }
  if(FGameStatus == gsWon) {
     Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
     if(ACellState.SurroundNumber == -1)
       FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBombMark, true);  // bomb flag
  }
}

void __fastcall TIntMinerField::DrawBombMarkedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState)
{
  if(FGameStatus == gsLost) {
    if(ACellState.SurroundNumber == -1) {
      Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBombMark, true);  // bomb flag
    }
    else {
      // striked out bomb on an empty background
      ACanvas->Brush->Color = FOpenCellBkColor;
      ACanvas->FillRect(ARect);
      ACanvas->Pen->Style = psSolid;
      ACanvas->Pen->Color = FRectangleColor;
      ARect.Left--; ARect.Top--;
      ACanvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      ARect.Left = ARect.Left + 2; ARect.Top = ARect.Top + 2;
      ACanvas->Brush->Style = bsClear;
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imStruckOutBomb, true);
    }
  } else
  if((FGameStatus == gsRun) || (FGameStatus == gsWon) || (FGameStatus == gsNew)) {
    Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
    FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBombMark, true);  // bomb flag
  }
}

void __fastcall TIntMinerField::DrawQuestionMarkedCell(TCanvas* ACanvas, TRect ARect, TCellStateRec ACellState, int ACol, int ARow)
{
  if(FGameStatus == gsWon) {
    if(ACellState.SurroundNumber == -1) {
      Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressed
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBombMark, true);  // bomb flag
    }
  } else
  if(FGameStatus == gsLost) {
    if(ACellState.SurroundNumber == -1) {
      ACanvas->Brush->Color = FClosedCellBkColor;
      ACanvas->FillRect(ARect);
      ACanvas->Pen->Style = psSolid;
      ACanvas->Pen->Color = FOpenCellBkColor;
      ARect.Left--; ARect.Top--;
      ACanvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      ARect.Left = ARect.Left + 2; ARect.Top = ARect.Top + 2;
      ACanvas->Brush->Style = bsClear;
      FImages->Draw(ACanvas, ARect.Left, ARect.Top, imBomb, true); // bomb on an empty background
   } else {
     Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpressesd
     FImages->Draw(ACanvas, ARect.Left, ARect.Top, imQuestionMark, true);      // question mark
   }
   } else
   if((FGameStatus == gsNew) || (FGameStatus == gsRun)) {
     if(!IsExistsInArray(FPressedCells, ACol, ARow))
       Frame3D(ACanvas, ARect, FFrameColor, FOpenCellBkColor, 1); // unpresses
     else
       Frame3D(ACanvas, ARect, FOpenCellBkColor, FFrameColor, 1); // pressed
     FImages->Draw(ACanvas, ARect.Left, ARect.Top, imQuestionMark, true);      // question mark
   }
}

void __fastcall TIntMinerField::DrawCell(const TCellStateRec ACellState, int ACol, int ARow, const TRect &ARect, Graphics::TCanvas* ACanvas)
{
  String CellStr;
  ACanvas->Brush->Style = bsSolid;
  ACanvas->Brush->Color = FClosedCellBkColor;
  ACanvas->FillRect(ARect);
  switch (ACellState.CellState) {
    case csOpened: DrawOpenedCell(ACanvas, ARect, ACellState); break;
    case csClosed: DrawClosedCell(ACanvas, ARect, ACellState, ACol, ARow); break;
    case csBombMarked: DrawBombMarkedCell(ACanvas, ARect, ACellState); break;
    case csQuestionMarked: DrawQuestionMarkedCell(ACanvas, ARect, ACellState, ACol, ARow); break;
  };
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::InvalidateCell(int ACol, int ARow)
{
  TRect InvalidRect = ((TcxGridTableView*)FGrid->Views[0])->ViewInfo->RecordsViewInfo->Items[ARow]->GetCellViewInfoByItem(((TcxGridTableView*)FGrid->Views[0])->Items[ACol])->Bounds;
  FGrid->Views[0]->Painter->Invalidate(InvalidRect);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::MouseDownHandler(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y)
{
  int ACol, ARow;

  SetCaptureControl(FGrid->Views[0]->Site);
  MouseToCell(X, Y, ACol, ARow);
  if((ACol == - 1) || (ARow == -1))
    return;

  if((Shift == (TShiftState() << ssLeft << ssRight)) || (Shift == (TShiftState() << ssLeft << ssRight))
    || (Shift == (TShiftState() << ssMiddle)) || (Shift == (TShiftState() << ssLeft << ssMiddle))
    || (Shift == (TShiftState() << ssRight << ssMiddle))) {
    FireImageChanged(imAstonisment);
    FSurprised = true;
    for(int i=-1; i <=1; i++)
      for(int j=-1; j <=1; j++)
          if(CheckFieldBounds(ACol+i, ARow+j) && (
            (CellState[ACol+i][ARow+j].CellState == csClosed)
            || (CellState[ACol+i][ARow+j].CellState == csQuestionMarked))) {
            AddPressedCell(ACol+i, ARow+j);
            InvalidateCell(ACol+i, ARow+j);
          };
    return;
  };

  if(Button == mbLeft) {
    FireImageChanged(imAstonisment);
    FSurprised = true;
    if((CellState[ACol][ARow].CellState == csClosed)
        || (CellState[ACol][ARow].CellState == csQuestionMarked)) {
        AddPressedCell(ACol, ARow);
        InvalidateCell(ACol, ARow);
      };
    return;
  };

  if(Button == mbRight) {
    if(CellState[ACol][ARow].CellState == csOpened) return;
    if(CellState[ACol][ARow].CellState == csClosed) {
      FireMinerFieldEvent(ACol, ARow, meBombMarkCell);
      FireSetMineCountEvent(mcDecMineCount);
    }
    else
    if((CellState[ACol][ARow].CellState == csBombMarked) &&
      FQuestionMarkCell) {
      FireMinerFieldEvent(ACol, ARow, meQuestionMarkCell);
      FireSetMineCountEvent(mcIncMineCount);
    }
    else {
      FireMinerFieldEvent(ACol, ARow, meCloseCell);
      if(!FQuestionMarkCell) FireSetMineCountEvent(mcIncMineCount);
    };
    InvalidateCell(ACol, ARow);
  };
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::MouseMoveHandler(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y)
{
  int ACol, ARow;
  MouseToCell(X, Y, ACol, ARow);
  if((ACol == - 1) || (ARow == -1)) {
    if(FPressedCells.Length != 0)
      UnpressAndInvalidate();
    if(FSurprised) {
      FireImageChanged(imSmile);
      FSurprised = false;
    };
    return;
  };
  if(Shift == (TShiftState() << ssLeft)) {
    if((CellState[ACol][ARow].CellState == csOpened)) {
      if(FPressedCells.Length != 0) {
        UnpressAndInvalidate();
        InvalidateCell(ACol, ARow);
      };
    } else
    if((CellState[ACol][ARow].CellState == csClosed)
      || (CellState[ACol][ARow].CellState == csQuestionMarked))
      if(!IsExistsInArray(FPressedCells, ACol, ARow)) {
        UnpressAndInvalidate();
        AddPressedCell(ACol, ARow);
        InvalidateCell(ACol, ARow);
      }
    return;
  }

  if((Shift == (TShiftState() << ssLeft << ssRight)) || (Shift == (TShiftState() << ssLeft << ssRight))
    || (Shift == (TShiftState() << ssMiddle)) || (Shift == (TShiftState() << ssLeft << ssMiddle))
    || (Shift == (TShiftState() << ssRight << ssMiddle))) {
    UnpressAndInvalidate();
    for(int i=-1; i<=1; i++)
      for(int j=-1; j<=1; j++)
        if(CheckFieldBounds(ACol+i, ARow+j) &&
          ((CellState[ACol+i][ARow+j].CellState == csClosed) ||
          (CellState[ACol+i][ARow+j].CellState == csQuestionMarked))) {
            AddPressedCell(ACol+i, ARow+j);
            InvalidateCell(ACol+i, ARow+j);
          }
   }
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::MouseUpHandler(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y)
{
  int ACol, ARow;
  SetCaptureControl(NULL);
  MouseToCell(X, Y, ACol, ARow);
  if((ACol == - 1) || (ARow == -1))
    return;
  if(((Shift == (TShiftState() << ssLeft)) && (Button == mbRight)) ||
    ((Shift == (TShiftState() << ssRight)) && (Button == mbLeft))
     || (Button == mbMiddle)) {
    FireImageChanged(imSmile);
    FSurprised = false;
    UnpressAndInvalidate();
    if(CellState[ACol][ARow].CellState == csOpened)
      FireMinerFieldEvent(ACol, ARow, meCheckSurround);
    return;
  };
  if(Button == mbLeft) {
    FireImageChanged(imSmile);
    FSurprised = false;
    if(IsExistsInArray(FPressedCells, ACol, ARow)) {
//      BeginUpdate();
      FireMinerFieldEvent(ACol, ARow, meOpenCell);
//      EndUpdate();
      UnpressAndInvalidate();
    };
    return;
  };
  if(Button == mbRight)
    UnpressAndInvalidate();
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::HandleEvMinerFieldChanged(System::TObject* Sender, TCells &AChangedCells, TCells &ARedCells)
{
  InvalidateCells(AChangedCells, ARedCells);
//  AChangedCells.Length = 0;
//  ARedCells.Length = 0;
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::HandleEvGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty, TCells &AChangedCells, TCells &ARedCells)
{
  switch (AGameStatus) {
    case gsNew: {
      FGameStatus = gsNew;
      FGameDifficulty = AGameDifficulty;
      InitNewGame();
    }; break;
    case gsRun: FGameStatus = gsRun; break;
    case gsLost: {
      FGameStatus = gsLost;
      UpdateMinerFieldState(ARedCells);
      InvalidateCells(AChangedCells, ARedCells);
      FGrid->Views[0]->Painter->Invalidate();
	  FGrid->Enabled = false;
    }; break;
    case gsWon: {
      FGameStatus = gsWon;
      UpdateMinerFieldState(ARedCells);
      InvalidateCells(AChangedCells, ARedCells);
      FGrid->Views[0]->Painter->Invalidate();
	  FGrid->Enabled = false;
    }; break;
  };
  ARedCells.Length = 0;
  AChangedCells.Length = 0;
  FireGameStatusChanged(Sender, AGameStatus, AGameDifficulty);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::SetColorScheme(const UnboundModeDemoTypesH::TColorScheme Value)
{
  FColorScheme = Value;
  SetSchemeColors();
  FGrid->Views[0]->Painter->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::SetNumberColors(void)
{
  FSurroundColors.Length = 8;
  FSurroundColors[0] = clBlue;
  FSurroundColors[1] = clGreen;
  FSurroundColors[2] = clRed;
  FSurroundColors[3] = clNavy;
  FSurroundColors[4] = clPurple;
  FSurroundColors[5] = clBlue;
  FSurroundColors[6] = clBlue;
  FSurroundColors[7] = clGray;
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::FireMinerFieldEvent(int ACol, int ARow, TMinerFieldActionEventType AMinerFieldActionEventType)
{
  if(FOnMinerFieldAction != NULL)
    FOnMinerFieldAction(this, ACol, ARow, AMinerFieldActionEventType);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::FireNewGameEvent(void)
{
  if(FCreateNewGameEvent != NULL)
    FCreateNewGameEvent(this);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::FireImageChanged(int AImageIndex)
{
  if(FOnImageChanged != NULL)
    FOnImageChanged(this, AImageIndex);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::FireSetMineCountEvent(TMineCountChangedEventType AMineCountChangedEventType)
{
  if(FMineCountChanged != NULL)
    FMineCountChanged(this, AMineCountChangedEventType);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::FireGameStatusChanged(System::TObject* Sender, TGameStatus AGameStatus, const TGameDifficulty &AGameDifficulty)
{
  if(FGameStatusChanged != NULL)
    FGameStatusChanged(Sender, AGameStatus, AGameDifficulty);
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::MouseToCell(int X, int Y, int &ACol, int &ARow)
{
  TcxCustomGridHitTest* AHitTest;
  ACol = -1;
  ARow = -1;
  AHitTest = FGrid->ViewInfo->GetHitTest(X, Y);
  if(dynamic_cast<TcxGridRecordCellHitTest*>(AHitTest)) {
    ACol = ((TcxGridRecordCellHitTest*)AHitTest)->Item->Index;
    ARow = ((TcxGridRecordCellHitTest*)AHitTest)->GridRecord->Index;
  }
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::CustomDrawCellHandler(Cxgridcustomtableview::TcxCustomGridTableView* Sender, Cxgraphics::TcxCanvas* ACanvas, Cxgridcustomtableview::TcxGridTableDataCellViewInfo* AViewInfo, bool &ADone)
{
  TCellStateRec CellRec;
  int ACol = AViewInfo->Item->Index;
  int ARow = AViewInfo->RecordViewInfo->Index;
  TRect DrawRect = AViewInfo->Bounds;
  int vType = VarType(AViewInfo->Value);
  if((vType == varEmpty) || (vType == varNull)) return;
  CellRec = *(PCellStateRec)(int)AViewInfo->Value;
  DrawCell(CellRec, ACol, ARow, DrawRect, ACanvas->Canvas);
  ADone = true;
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::SetGridViewOptions(TcxGridTableView* GridView)
{
  GridView->OptionsData->Editing = false;
  GridView->OptionsData->Inserting = false;
  GridView->OptionsData->Deleting = false;
  GridView->OptionsView->GroupByBox = false;
  GridView->OptionsView->GridLines = glNone;
  GridView->OptionsView->FocusRect = false;
  GridView->OptionsView->ScrollBars = ssNone;

  GridView->OptionsSelection->CellSelect = false;
  GridView->OptionsSelection->HideSelection = false;
  GridView->OptionsSelection->InvertSelect = false;
  GridView->OptionsView->Header = false;
}
//---------------------------------------------------------------------------

__fastcall TIntMinerField::TIntMinerField(Classes::TComponent* AOwner, TMinerField* AMinerField) : TObject()
{
  FGrid = new TcxCustomGrid(AOwner);
  FGrid->LookAndFeel->NativeStyle = false;
  TcxGridTableView* GridView = (TcxGridTableView*)FGrid->CreateView(__classid(TcxGridTableView));

  TcxGridLevel* Level = FGrid->Levels->Add();
  Level->GridView = GridView;

  FMinerFieldDataSource = new TMinerFieldDataSource();
  MinerField = AMinerField;
  MinerField->OnMinerFieldChanged = FMinerFieldDataSource->HandleEvMinerFieldChanged;
  MinerField->OnGameStatusChanged = FMinerFieldDataSource->HandleEvGameStatusChanged;

  FMinerFieldDataSource->OnMinerFieldChanged = HandleEvMinerFieldChanged;
  FMinerFieldDataSource->OnGameStatusChanged = HandleEvGameStatusChanged;

  GridView->DataController->CustomDataSource = FMinerFieldDataSource;
  SetGridViewOptions(GridView);

  GridView->OnCustomDrawCell = CustomDrawCellHandler;

  GridView->OnMouseDown = MouseDownHandler;
  GridView->OnMouseUp = MouseUpHandler;
  GridView->OnMouseMove = MouseMoveHandler;

  OnMinerFieldAction = MinerField->HandleMinerFieldActionEvent;

  FCellWidth = GridView->ViewInfo->RecordsViewInfo->RowHeight;
  FColorScheme = csBlue;
  SetNumberColors();
  SetSchemeColors();
}
//---------------------------------------------------------------------------

__fastcall TIntMinerField::~TIntMinerField(void)
{
  FOnMinerFieldAction = NULL;
  FMineCountChanged = NULL;
  FOnImageChanged = NULL;
  FGameStatusChanged = NULL;
  FPressedCells.Length = 0;
  FRedCells.Length = 0;
  delete FMinerFieldDataSource;
}
//---------------------------------------------------------------------------

void __fastcall TIntMinerField::CreateNewGame(void)
{
  FireNewGameEvent();
  InitNewGame();
}
//---------------------------------------------------------------------------
void __fastcall TIntMinerField::SetParent(TWinControl* AParent)
{
  FGrid->Parent = AParent;
  FGrid->Visible = true;
}
//---------------------------------------------------------------------------
