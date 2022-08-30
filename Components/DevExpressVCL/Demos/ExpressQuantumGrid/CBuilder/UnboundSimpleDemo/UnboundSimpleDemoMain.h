//---------------------------------------------------------------------------

#ifndef UnboundSimpleDemoMainH
#define UnboundSimpleDemoMainH
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
#include "cxEditRepositoryItems.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxFormats.hpp"
//---------------------------------------------------------------------------
class TUnboundSimpleDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridTableView *tvPlanets;
  TcxGridColumn *tvPlanetsNAME;
  TcxGridColumn *tvPlanetsNO;
  TcxGridColumn *tvPlanetsORBITS;
  TcxGridColumn *tvPlanetsDISTANCE;
  TcxGridColumn *tvPlanetsPERIOD;
  TcxGridColumn *tvPlanetsDISCOVERER;
  TcxGridColumn *tvPlanetsDATE;
  TcxGridColumn *tvPlanetsRADIUS;
  TcxGridLevel *lvPlanets;
  TcxEditRepository *edrepMain;
  TcxEditRepositoryTextItem *edrepCenterText;
  TcxEditRepositoryTextItem *edrepRightText;
  void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
  void __fastcall CustomizeColumns();
  void __fastcall LoadData();
  void __fastcall SetFilter();
  void __fastcall InitRecord(String const Str, int AInt, TStringList* AValues);
public:   // User declarations
  __fastcall TUnboundSimpleDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundSimpleDemoMainForm *UnboundSimpleDemoMainForm;
//---------------------------------------------------------------------------
#endif
