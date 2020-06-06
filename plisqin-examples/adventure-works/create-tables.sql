create table Address (
  AddressID integer_id not null
  , AddressLine1 text not null
  , AddressLine2 text not null
  , City text not null
  , StateProvinceID integer_id not null
  , PostalCode text not null
  , SpatialLocation text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table AddressType (
  AddressTypeID integer_id not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table AWBuildVersion (
  SystemInformationID integer_id not null
  , DatabaseVersion text not null
  , VersionDate text not null
  , ModifiedDate text not null
);

create table BillOfMaterials (
  BillOfMaterialsID integer_id not null
  , ProductAssemblyID integer_id not null
  , ComponentID integer_id not null
  , StartDate text not null
  , EndDate text not null
  , UnitMeasureCode text not null
  , BOMLevel text not null
  , PerAssemblyQty text not null
  , ModifiedDate text not null
);

create table BusinessEntity (
  BusinessEntityID integer_id not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table BusinessEntityAddress (
  BusinessEntityID integer_id not null
  , AddressID integer_id not null
  , AddressTypeID integer_id not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table BusinessEntityContact (
  BusinessEntityID integer_id not null
  , PersonID integer_id not null
  , ContactTypeID integer_id not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ContactType (
  ContactTypeID integer_id not null
  , Name text not null
  , ModifiedDate text not null
);

create table CountryRegion (
  CountryRegionCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table CountryRegionCurrency (
  CountryRegionCode text not null
  , CurrencyCode text not null
  , ModifiedDate text not null
);

create table CreditCard (
  CreditCardID integer_id not null
  , CardType text not null
  , CardNumber text not null
  , ExpMonth text not null
  , ExpYear text not null
  , ModifiedDate text not null
);

create table Culture (
  CultureID integer_id not null
  , Name text not null
  , ModifiedDate text not null
);

create table Currency (
  CurrencyCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table CurrencyRate (
  CurrencyRateID integer_id not null
  , CurrencyRateDate text not null
  , FromCurrencyCode text not null
  , ToCurrencyCode text not null
  , AverageRate text not null
  , EndOfDayRate text not null
  , ModifiedDate text not null
);

create table Customer (
  CustomerID integer_id not null
  , PersonID integer_id not null
  , StoreID integer_id not null
  , TerritoryID integer_id not null
  , AccountNumber text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table DatabaseLog (
  DatabaseLogID integer_id not null
  , PostTime text not null
  , DatabaseUser text not null
  , Event text not null
  , Schema text not null
  , Object text not null
  , TSQL text not null
  , XmlEvent text not null
);

create table Department (
  DepartmentID integer_id not null
  , Name text not null
  , GroupName text not null
  , ModifiedDate text not null
);

create table Document (
  DocumentNode text not null
  , DocumentLevel text not null
  , Title text not null
  , Owner text not null
  , FolderFlag text not null
  , FileName text not null
  , FileExtension text not null
  , Revision text not null
  , ChangeNumber text not null
  , Status text not null
  , DocumentSummary text not null
  , Document text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table EmailAddress (
  BusinessEntityID integer_id not null
  , EmailAddressID integer_id not null
  , EmailAddress text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Employee (
  BusinessEntityID integer_id not null
  , NationalIDNumber text not null
  , LoginID text not null
  , OrganizationNode text not null
  , OrganizationLevel text not null
  , JobTitle text not null
  , BirthDate text not null
  , MaritalStatus text not null
  , Gender text not null
  , HireDate text not null
  , SalariedFlag text not null
  , VacationHours text not null
  , SickLeaveHours text not null
  , CurrentFlag text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table EmployeeDepartmentHistory (
  BusinessEntityID integer_id not null
  , DepartmentID integer_id not null
  , ShiftID integer_id not null
  , StartDate text not null
  , EndDate text not null
  , ModifiedDate text not null
);

create table EmployeePayHistory (
  BusinessEntityID integer_id not null
  , RateChangeDate text not null
  , Rate text not null
  , PayFrequency text not null
  , ModifiedDate text not null
);

create table ErrorLog (
  ErrorLogID integer_id not null
  , ErrorTime text not null
  , UserName text not null
  , ErrorNumber text not null
  , ErrorSeverity text not null
  , ErrorState text not null
  , ErrorProcedure text not null
  , ErrorLine text not null
  , ErrorMessage text not null
);

create table Illustration (
  IllustrationID integer_id not null
  , Diagram text not null
  , ModifiedDate text not null
);

create table JobCandidate (
  JobCandidateID integer_id not null
  , BusinessEntityID integer_id not null
  , Resume text not null
  , ModifiedDate text not null
);

create table Location (
  LocationID integer_id not null
  , Name text not null
  , CostRate text not null
  , Availability text not null
  , ModifiedDate text not null
);

create table Password (
  BusinessEntityID integer_id not null
  , PasswordHash text not null
  , PasswordSalt text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Person (
  BusinessEntityID integer_id not null
  , PersonType text not null
  , NameStyle text not null
  , Title text not null
  , FirstName text not null
  , MiddleName text not null
  , LastName text not null
  , Suffix text not null
  , EmailPromotion text not null
  , AdditionalContactInfo text not null
  , Demographics text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table PersonCreditCard (
  BusinessEntityID integer_id not null
  , CreditCardID integer_id not null
  , ModifiedDate text not null
);

create table PersonPhone (
  BusinessEntityID integer_id not null
  , PhoneNumber text not null
  , PhoneNumberTypeID integer_id not null
  , ModifiedDate text not null
);

create table PhoneNumberType (
  PhoneNumberTypeID integer_id not null
  , Name text not null
  , ModifiedDate text not null
);

create table Product (
  ProductID integer_id not null
  , Name text not null
  , ProductNumber text not null
  , MakeFlag text not null
  , FinishedGoodsFlag text not null
  , Color text null
  , SafetyStockLevel integer not null
  , ReorderPoint text not null
  , StandardCost text not null
  , ListPrice real not null
  , Size text null
  , SizeUnitMeasureCode text null
  , WeightUnitMeasureCode text null
  , Weight text null
  , DaysToManufacture text not null
  , ProductLine text null
  , Class text null
  , Style text null
  , ProductSubcategoryID integer_id null
  , ProductModelID integer_id null
  , SellStartDate text not null
  , SellEndDate text null
  , DiscontinuedDate text null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductCategory (
  ProductCategoryID integer_id not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductCostHistory (
  ProductID integer_id not null
  , StartDate text not null
  , EndDate text not null
  , StandardCost text not null
  , ModifiedDate text not null
);

create table ProductDescription (
  ProductDescriptionID integer_id not null
  , Description text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductDocument (
  ProductID integer_id not null
  , DocumentNode text not null
  , ModifiedDate text not null
);

create table ProductInventory (
  ProductID integer_id not null
  , LocationID integer_id not null
  , Shelf text not null
  , Bin text not null
  , Quantity text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductListPriceHistory (
  ProductID integer_id not null
  , StartDate text not null
  , EndDate text not null
  , ListPrice real not null
  , ModifiedDate text not null
);

create table ProductModel (
  ProductModelID integer_id not null
  , Name text not null
  , CatalogDescription text not null
  , Instructions text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductModelIllustration (
  ProductModelID integer_id not null
  , IllustrationID integer_id not null
  , ModifiedDate text not null
);

create table ProductModelProductDescriptionCulture (
  ProductModelID integer_id not null
  , ProductDescriptionID integer_id not null
  , CultureID integer_id not null
  , ModifiedDate text not null
);

create table ProductPhoto (
  ProductPhotoID integer_id not null
  , ThumbNailPhoto text not null
  , ThumbnailPhotoFileName text not null
  , LargePhoto text not null
  , LargePhotoFileName text not null
  , ModifiedDate text not null
);

create table ProductProductPhoto (
  ProductID integer_id not null
  , ProductPhotoID integer_id not null
  , [Primary] text not null
  , ModifiedDate text not null
);

create table ProductReview (
  ProductReviewID integer_id not null
  , ProductID integer_id not null
  , ReviewerName text not null
  , ReviewDate text not null
  , EmailAddress text not null
  , Rating text not null
  , Comments text not null
  , ModifiedDate text not null
);

create table ProductSubcategory (
  ProductSubcategoryID integer_id not null
  , ProductCategoryID integer_id not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ProductVendor (
  ProductID integer_id not null
  , BusinessEntityID integer_id not null
  , AverageLeadTime text not null
  , StandardPrice text not null
  , LastReceiptCost text not null
  , LastReceiptDate text not null
  , MinOrderQty text not null
  , MaxOrderQty text not null
  , OnOrderQty text not null
  , UnitMeasureCode text not null
  , ModifiedDate text not null
);

create table PurchaseOrderDetail (
  PurchaseOrderID integer_id not null
  , PurchaseOrderDetailID integer_id not null
  , DueDate text not null
  , OrderQty text not null
  , ProductID integer_id not null
  , UnitPrice text not null
  , LineTotal text not null
  , ReceivedQty text not null
  , RejectedQty text not null
  , StockedQty text not null
  , ModifiedDate text not null
);

create table PurchaseOrderHeader (
  PurchaseOrderID integer_id not null
  , RevisionNumber text not null
  , Status text not null
  , EmployeeID integer_id not null
  , VendorID integer_id not null
  , ShipMethodID integer_id not null
  , OrderDate text not null
  , ShipDate text not null
  , SubTotal text not null
  , TaxAmt text not null
  , Freight text not null
  , TotalDue text not null
  , ModifiedDate text not null
);

create table SalesOrderDetail (
  SalesOrderID integer_id not null
  , SalesOrderDetailID integer_id not null
  , CarrierTrackingNumber text null
  , OrderQty text not null
  , ProductID integer_id not null
  , SpecialOfferID integer_id not null
  , UnitPrice real not null
  , UnitPriceDiscount real not null
  , LineTotal text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesOrderHeader (
  SalesOrderID integer_id not null
  , RevisionNumber text not null
  , OrderDate text not null
  , DueDate text not null
  , ShipDate text not null
  , Status text not null
  , OnlineOrderFlag text not null
  , SalesOrderNumber text not null
  , PurchaseOrderNumber text null
  , AccountNumber text not null
  , CustomerID integer_id not null
  , SalesPersonID integer_id null
  , TerritoryID integer_id not null
  , BillToAddressID integer_id not null
  , ShipToAddressID integer_id not null
  , ShipMethodID integer_id not null
  , CreditCardID integer_id null
  , CreditCardApprovalCode text null
  , CurrencyRateID integer_id null
  , SubTotal text not null
  , TaxAmt text not null
  , Freight text not null
  , TotalDue text not null
  , Comment text null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesOrderHeaderSalesReason (
  SalesOrderID integer_id not null
  , SalesReasonID integer_id not null
  , ModifiedDate text not null
);

create table SalesPerson (
  BusinessEntityID integer_id not null
  , TerritoryID integer_id not null
  , SalesQuota text not null
  , Bonus text not null
  , CommissionPct text not null
  , SalesYTD text not null
  , SalesLastYear text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesPersonQuotaHistory (
  BusinessEntityID integer_id not null
  , QuotaDate text not null
  , SalesQuota text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesReason (
  SalesReasonID integer_id not null
  , Name text not null
  , ReasonType text not null
  , ModifiedDate text not null
);

create table SalesTaxRate (
  SalesTaxRateID integer_id not null
  , StateProvinceID integer_id not null
  , TaxType text not null
  , TaxRate text not null
  , Name text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesTerritory (
  TerritoryID integer_id not null
  , Name text not null
  , CountryRegionCode text not null
  , [Group] text not null
  , SalesYTD text not null
  , SalesLastYear text not null
  , CostYTD text not null
  , CostLastYear text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SalesTerritoryHistory (
  BusinessEntityID integer_id not null
  , TerritoryID integer_id not null
  , StartDate text not null
  , EndDate text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ScrapReason (
  ScrapReasonID integer_id not null
  , Name text not null
  , ModifiedDate text not null
);

create table Shift (
  ShiftID integer_id not null
  , Name text not null
  , StartTime text not null
  , EndTime text not null
  , ModifiedDate text not null
);

create table ShipMethod (
  ShipMethodID integer_id not null
  , Name text not null
  , ShipBase text not null
  , ShipRate text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table ShoppingCartItem (
  ShoppingCartItemID integer_id not null
  , ShoppingCartID integer_id not null
  , Quantity text not null
  , ProductID integer_id not null
  , DateCreated text not null
  , ModifiedDate text not null
);

create table SpecialOffer (
  SpecialOfferID integer_id not null
  , Description text not null
  , DiscountPct real not null
  , Type text not null
  , Category text not null
  , StartDate text not null
  , EndDate text not null
  , MinQty real not null
  , MaxQty real null
  , rowguid text not null
  , ModifiedDate text not null
);

create table SpecialOfferProduct (
  SpecialOfferID integer_id not null
  , ProductID integer_id not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table StateProvince (
  StateProvinceID integer_id not null
  , StateProvinceCode text not null
  , CountryRegionCode text not null
  , IsOnlyStateProvinceFlag text not null
  , Name text not null
  , TerritoryID integer_id not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table Store (
  BusinessEntityID integer_id not null
  , Name text not null
  , SalesPersonID integer_id not null
  , Demographics text not null
  , rowguid text not null
  , ModifiedDate text not null
);

create table TransactionHistory (
  TransactionID integer_id not null
  , ProductID integer_id not null
  , ReferenceOrderID integer_id not null
  , ReferenceOrderLineID integer_id not null
  , TransactionDate text not null
  , TransactionType text not null
  , Quantity text not null
  , ActualCost text not null
  , ModifiedDate text not null
);

create table TransactionHistoryArchive (
  TransactionID integer_id not null
  , ProductID integer_id not null
  , ReferenceOrderID integer_id not null
  , ReferenceOrderLineID integer_id not null
  , TransactionDate text not null
  , TransactionType text not null
  , Quantity text not null
  , ActualCost text not null
  , ModifiedDate text not null
);

create table UnitMeasure (
  UnitMeasureCode text not null
  , Name text not null
  , ModifiedDate text not null
);

create table Vendor (
  BusinessEntityID integer_id not null
  , AccountNumber text not null
  , Name text not null
  , CreditRating text not null
  , PreferredVendorStatus text not null
  , ActiveFlag text not null
  , PurchasingWebServiceURL text not null
  , ModifiedDate text not null
);

create table WorkOrder (
  WorkOrderID integer_id not null
  , ProductID integer_id not null
  , OrderQty text not null
  , StockedQty text not null
  , ScrappedQty text not null
  , StartDate text not null
  , EndDate text not null
  , DueDate text not null
  , ScrapReasonID integer_id not null
  , ModifiedDate text not null
);

create table WorkOrderRouting (
  WorkOrderID integer_id not null
  , ProductID integer_id not null
  , OperationSequence text not null
  , LocationID integer_id not null
  , ScheduledStartDate text not null
  , ScheduledEndDate text not null
  , ActualStartDate text not null
  , ActualEndDate text not null
  , ActualResourceHrs text not null
  , PlannedCost text not null
  , ActualCost text not null
  , ModifiedDate text not null
);