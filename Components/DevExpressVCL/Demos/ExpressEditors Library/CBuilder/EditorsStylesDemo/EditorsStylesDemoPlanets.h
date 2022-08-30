//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoPlanetsH
#define EditorsStylesDemoPlanetsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include "EditorsStylesDemoBase.h"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxLabel.hpp"
#include "cxListView.hpp"
#include "cxMCListBox.hpp"
#include "cxSplitter.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TEditorsStylesDemoPlanetsFrame : public TEditorsStylesDemoBaseFrame
{
__published:	// IDE-managed Components
  TPanel *pnlPlanets;
  TPanel *pnlPlanetsLbl;
  TcxLabel *lblPlanets;
  TcxMCListBox *cxMCListBox;
  TPanel *pnlSatellites;
  TPanel *pnlSatellitesLbl;
  TcxLabel *lblSatellites;
  TcxListView *cxListView;
  TcxSplitter *cxSplitter;
  TImageList *ImageList;
  void __fastcall cxMCListBoxClick(TObject *Sender);
  void __fastcall cxListViewInfoTip(TObject *Sender, TListItem *Item,
          String &InfoTip);
private:
  TStringList* FRecordValues;
  void __fastcall UpdateSatellites();
  String __fastcall GetValue(int ARecordIndex, int AColIndex);
  int __fastcall GetIndexByName(String AName);
  void InitCurrentRecordValues(String ARecord);
public:		// User declarations
  String __fastcall Name();
  String __fastcall BriefName();
  __fastcall TEditorsStylesDemoPlanetsFrame(TComponent* Owner);
  __fastcall ~TEditorsStylesDemoPlanetsFrame();
  String StylesIniPath();
  TColor GetStyleBackgroundColor();
  String Description();
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoPlanetsFrame *EditorsStylesDemoPlanetsFrame;
//---------------------------------------------------------------------------
#endif
