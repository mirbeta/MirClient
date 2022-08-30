//---------------------------------------------------------------------------

#ifndef EQGridRLMainH
#define EQGridRLMainH
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
#include "dxPSCore.hpp"
#include "dxPScxGridLnk.hpp"
#include <ToolWin.hpp>
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include "cxGridCustomView.hpp"
#include "cxDataStorage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxDrawTextUtils.hpp"
#include "DemoBasicMain.h"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "dxPScxCommon.hpp"
//---------------------------------------------------------------------------
class TEQGridRLMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
        TAction *actFullExpand;
        TAction *actFullCollapse;
        TcxStyleRepository *StyleRepository;
        TcxStyle *cxStyle1;
        TcxStyle *cxStyle2;
        TcxStyle *cxStyle3;
        TcxStyle *cxStyle4;
        TcxStyle *cxStyle5;
        TcxStyle *cxStyle6;
        TcxStyle *cxStyle7;
        TcxStyle *cxStyle8;
        TcxStyle *cxStyle9;
        TcxStyle *cxStyle10;
        TcxStyle *cxStyle11;
        TcxStyle *cxStyle12;
        TcxStyle *cxStyle13;
        TcxStyle *cxStyle14;
        TcxGridTableViewStyleSheet *tvssDevExpress;
        TcxEditRepository *edrepMain;
        TcxEditRepositoryTextItem *edrepCenterText;
        TcxEditRepositoryTextItem *edrepRightText;
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
  void __fastcall actLookAndFeelKindUltraFlatExecute(TObject *Sender);
  void __fastcall actLookAndFeelKindFlatExecute(TObject *Sender);
  void __fastcall actLookAndFeelKindStandardExecute(TObject *Sender);
  void __fastcall miNativeStyleClick(TObject *Sender);
  void __fastcall actShowDemoDescriptionExecute(TObject *Sender);
  void __fastcall actGridNativeStyleExecute(TObject *Sender);
  void __fastcall actDownloadsExecute(TObject *Sender);
  void __fastcall actForumExecute(TObject *Sender);
  void __fastcall actDXOnTheWebExecute(TObject *Sender);
  void __fastcall actProductsExecute(TObject *Sender);
  void __fastcall AlwaysEnabled(TObject *Sender);
  void __fastcall actExitExecute(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall actFullExpandExecute(TObject *Sender);
  void __fastcall actFullCollapseExecute(TObject *Sender);
private:	// User declarations
  void __fastcall CustomizeColumns();
  void __fastcall LoadData();
  void __fastcall InitRecord(String const Str, int AInt, TStringList* AValues);
public:		// User declarations
  __fastcall TEQGridRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEQGridRLMainForm *EQGridRLMainForm;
//---------------------------------------------------------------------------
#endif
