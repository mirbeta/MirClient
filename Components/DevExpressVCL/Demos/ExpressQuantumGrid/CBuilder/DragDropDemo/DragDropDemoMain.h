//---------------------------------------------------------------------------

#ifndef DragDropDemoMainH
#define DragDropDemoMainH
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
#include "cxGridBandedTableView.hpp"
#include "cxGridCardView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMemo.hpp"
//---------------------------------------------------------------------------
class TDragDropDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBCardView *cvPersons;
  TcxGridDBCardViewRow *cvPersonsName;
  TcxGridDBCardViewRow *cvPersonsPersonLineID;
  TcxGridDBCardViewRow *cvPersonsFIRSTNAME;
  TcxGridDBCardViewRow *cvPersonsSECONDNAME;
  TcxGridDBCardViewRow *cvPersonsNICKNAME;
  TcxGridDBCardViewRow *cvPersonsBIRTHNAME;
  TcxGridDBCardViewRow *cvPersonsDATEOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsBIOGRAPHY;
  TcxGridDBCardViewRow *cvPersonsID;
  TcxGridDBCardViewRow *cvPersonsFilmID;
  TcxGridDBCardViewRow *cvPersonsBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvPersonsGender;
  TcxGridDBTableView *tvCompanies;
  TcxGridDBColumn *tvCompaniesID;
  TcxGridDBColumn *tvCompaniesFilmID;
  TcxGridDBColumn *tvCompaniesCompanyID;
  TcxGridDBColumn *tvCompaniesCompanyName;
  TcxGridDBCardView *cvPhotos;
  TcxGridDBCardViewRow *cvPhotosID;
  TcxGridDBCardViewRow *cvPhotosFILMID;
  TcxGridDBCardViewRow *cvPhotosSCREEN;
  TcxGridDBCardViewRow *cvPhotosICON;
  TcxGridDBBandedTableView *bvFilms;
  TcxGridDBBandedColumn *bvFilmsCAPTION;
  TcxGridDBBandedColumn *bvFilmsPHOTO;
  TcxGridDBBandedColumn *bvFilmsYEAR;
  TcxGridDBBandedColumn *bvFilmsRUNTIME;
  TcxGridDBBandedColumn *bvFilmsPLOTOUTLINE;
  TcxGridDBBandedColumn *bvFilmsWEBSITE;
  TMenuItem *miOptions;
  TMenuItem *miShowDictionaries;
  TMenuItem *N1;
  TMenuItem *miGenreTabPosition;
  TMenuItem *miTabPositionLeft;
  TMenuItem *miTabPositionTop;
  TMenuItem *miTabPositionNone;
  void __fastcall miShowDictionariesClick(TObject *Sender);
  void __fastcall miTabPositionClick(TObject *Sender);
  void __fastcall cvPersonsDragDrop(TObject *Sender, TObject *Source, int X, int Y);
  void __fastcall cvPersonsDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall tvCompaniesDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall tvCompaniesDragDrop(TObject *Sender, TObject *Source, int X, int Y);
  void __fastcall bvFilmsDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall bvFilmsDragDrop(TObject *Sender, TObject *Source, int X, int Y);
  void __fastcall GridActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel *ALevel);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall GridRootLevelStylesGetTabStyle(TcxGridLevel* Sender,
  	TcxGridLevel* ATabLevel, TcxStyle* &AStyle);
private:  // User declarations
  void __fastcall CreateLevels();
public:   // User declarations
  __fastcall TDragDropDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDragDropDemoMainForm *DragDropDemoMainForm;
//---------------------------------------------------------------------------
#endif
