/*******************************************************************************
 * Misc.
 */

// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
  Object.create = function( o ) {
    function F(){}
    F.prototype = o;
    return new F();
  };
}

// Insert properties of b in place into a.
function Fay$$objConcat(a,b){
  for (var p in b) if (b.hasOwnProperty(p)){
    a[p] = b[p];
  }
  return a;
}

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    return function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    return function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.

        // Unserialize the JS values to Fay for the Fay callback.
        if (args == "automatic_function")
        {
          for (var i = 0; i < arguments.length; i++) {
            fayFunc = Fay$$fayToJs(["automatic"], Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true));
          }
          return fayFunc;
        }

        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    return Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Undefined
      ? undefined
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Null
      ? null
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    return Fay$$_(fayObj);
  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" && fayObj instanceof Function) {
    return Fay$$fayToJs(["function", "automatic_function"], fayObj);
  }
  else if(base == "automatic" || base == "user") {
    fayObj = Fay$$_(fayObj);

    if(fayObj instanceof Fay$$Cons || fayObj === null){
      // Serialize Fay list to JavaScript array.
      var arr = [];
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(["automatic"],fayObj.car));
        fayObj = Fay$$_(fayObj.cdr);
      }
      return arr;
    } else {
      var fayToJsFun = fayObj && fayObj.instance && Fay$$fayToJsHash[fayObj.instance];
      return fayToJsFun ? fayToJsFun(type,type[2],fayObj) : fayObj;
    }
  }

  throw new Error("Unhandled Fay->JS translation type: " + base);
}

// Stores the mappings from fay types to js objects.
// This will be populated by compiled modules.
var Fay$$fayToJsHash = {};

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += Fay$$_(fayObj.car);
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "function") {
    // Unserialize a function from JavaScript to a function that Fay can call.
    // So
    //
    //    var f = function(x,y,z){ … }
    //
    // becomes something like:
    //
    //    function(x){
    //      return function(y){
    //        return function(z){
    //          return new Fay$$$(function(){
    //            return Fay$$jsToFay(f(Fay$$fayTojs(x),
    //                                  Fay$$fayTojs(y),
    //                                  Fay$$fayTojs(z))
    //    }}}}};
    var returnType = args[args.length-1];
    var funArgs = args.slice(0,-1);

    if (jsObj.length > 0) {
      var makePartial = function(args){
        return function(arg){
          var i = args.length;
          var fayArg = Fay$$fayToJs(funArgs[i],arg);
          var newArgs = args.concat([fayArg]);
          if(newArgs.length == funArgs.length) {
            return new Fay$$$(function(){
              return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs));
            });
          } else {
            return makePartial(newArgs);
          }
        };
      };
      return makePartial([]);
    }
    else
      return function (arg) {
        return Fay$$jsToFay(["automatic"], jsObj(Fay$$fayToJs(["automatic"], arg)));
      };
  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    // This is a special case, when String is explicit in the type signature,
    // with `Automatic' a string would not be decoded.
    return Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedList);
  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedTuple);
  }
  else if(base == "defined") {
    return jsObj === undefined
      ? new Fay.FFI._Undefined()
      : new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "nullable") {
    return jsObj === null
      ? new Fay.FFI._Null()
      : new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
    return fayObj;
  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" && jsObj instanceof Function) {
    var type = [["automatic"]];
    for (var i = 0; i < jsObj.length; i++)
      type.push(["automatic"]);
    return Fay$$jsToFay(["function", type], jsObj);
  }
  else if(base == "automatic" && jsObj instanceof Array) {
    var list = null;
    for (var i = jsObj.length - 1; i >= 0; i--) {
      list = new Fay$$Cons(Fay$$jsToFay([base], jsObj[i]), list);
    }
    return list;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      var jsToFayFun = Fay$$jsToFayHash[jsObj["instance"]];
      return jsToFayFun ? jsToFayFun(type,type[2],jsObj) : jsObj;
    }
    else
      return jsObj;
  }

  throw new Error("Unhandled JS->Fay translation type: " + base);
}

// Stores the mappings from js objects to fay types.
// This will be populated by compiled modules.
var Fay$$jsToFayHash = {};

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.instance === lit2.instance) {
    for(var x in lit1) {
      if(!Fay$$equal(lit1[x],lit2[x]))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Data = {};
Data.Data = {};
var Fay = {};
Fay.FFI = {};
Fay.FFI._Nullable = function Nullable(slot1){
  this.slot1 = slot1;
};
Fay.FFI._Nullable.prototype.instance = "Nullable";
Fay.FFI.Nullable = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Nullable(slot1);
  });
};
Fay.FFI._Null = function Null(){
};
Fay.FFI._Null.prototype.instance = "Null";
Fay.FFI.Null = new Fay$$$(function(){
  return new Fay.FFI._Null();
});
Fay.FFI._Defined = function Defined(slot1){
  this.slot1 = slot1;
};
Fay.FFI._Defined.prototype.instance = "Defined";
Fay.FFI.Defined = function(slot1){
  return new Fay$$$(function(){
    return new Fay.FFI._Defined(slot1);
  });
};
Fay.FFI._Undefined = function Undefined(){
};
Fay.FFI._Undefined.prototype.instance = "Undefined";
Fay.FFI.Undefined = new Fay$$$(function(){
  return new Fay.FFI._Undefined();
});
Fay$$objConcat(Fay$$fayToJsHash,{"Nullable": function(type,argTypes,_obj){
  var obj_ = {"instance": "Nullable"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Null": function(type,argTypes,_obj){
  var obj_ = {"instance": "Null"};
  return obj_;
},"Defined": function(type,argTypes,_obj){
  var obj_ = {"instance": "Defined"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Undefined": function(type,argTypes,_obj){
  var obj_ = {"instance": "Undefined"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Nullable": function(type,argTypes,obj){
  return new Fay.FFI._Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Null": function(type,argTypes,obj){
  return new Fay.FFI._Null();
},"Defined": function(type,argTypes,obj){
  return new Fay.FFI._Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Undefined": function(type,argTypes,obj){
  return new Fay.FFI._Undefined();
}});
var Prelude = {};
Prelude._Just = function Just(slot1){
  this.slot1 = slot1;
};
Prelude._Just.prototype.instance = "Just";
Prelude.Just = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Just(slot1);
  });
};
Prelude._Nothing = function Nothing(){
};
Prelude._Nothing.prototype.instance = "Nothing";
Prelude.Nothing = new Fay$$$(function(){
  return new Prelude._Nothing();
});
Prelude._Left = function Left(slot1){
  this.slot1 = slot1;
};
Prelude._Left.prototype.instance = "Left";
Prelude.Left = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Left(slot1);
  });
};
Prelude._Right = function Right(slot1){
  this.slot1 = slot1;
};
Prelude._Right.prototype.instance = "Right";
Prelude.Right = function(slot1){
  return new Fay$$$(function(){
    return new Prelude._Right(slot1);
  });
};
Prelude.maybe = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof Prelude._Nothing) {
          var m = $p1;
          return m;
        }
        if (Fay$$_($p3) instanceof Prelude._Just) {
          var x = Fay$$_($p3).slot1;
          var f = $p2;
          return Fay$$_(f)(x);
        }
        throw ["unhandled case in maybe",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.$62$$62$$61$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$_(Fay$$bind($p1)($p2));
    });
  };
};
Prelude.$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$_(Fay$$then($p1)($p2));
    });
  };
};
Prelude.$_return = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));
  });
};
Prelude.fail = new Fay$$$(function(){
  return Prelude.error;
});
Prelude.when = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var p = $p1;
      return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);
    });
  };
};
Prelude.unless = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var p = $p1;
      return Fay$$_(p) ? Fay$$_(Fay$$$_return)(Fay$$unit) : Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit));
    });
  };
};
Prelude.forM = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var fn = $p2;
      var lst = $p1;
      return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));
    });
  };
};
Prelude.forM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.forM_)(xs))(m));
      }
      if (Fay$$_($p1) === null) {
        return Fay$$_(Fay$$$_return)(Fay$$unit);
      }
      throw ["unhandled case in forM_",[$p1,$p2]];
    });
  };
};
Prelude.mapM = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var lst = $p2;
      var fn = $p1;
      return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));
    });
  };
};
Prelude.mapM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var m = $p1;
        return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.mapM_)(m))(xs));
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Fay$$$_return)(Fay$$unit);
      }
      throw ["unhandled case in mapM_",[$p1,$p2]];
    });
  };
};
Prelude.$61$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$bind)(x))(f);
    });
  };
};
Prelude.$_void = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(Fay$$_(Fay$$then)(f))(Fay$$_(Fay$$$_return)(Fay$$unit));
  });
};
Prelude.$62$$61$$62$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);
      });
    };
  };
};
Prelude.$60$$61$$60$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var f = $p2;
        var g = $p1;
        return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);
      });
    };
  };
};
Prelude.sequence = function($p1){
  return new Fay$$$(function(){
    var ms = $p1;
    return (function(){
      var k = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var m$39$ = $p2;
            var m = $p1;
            return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){
                var xs = $p1;
                return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));
              });
            });
          });
        };
      };
      return Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);
    })();
  });
};
Prelude.sequence_ = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Fay$$$_return)(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude.sequence_)(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
Prelude._GT = function GT(){
};
Prelude._GT.prototype.instance = "GT";
Prelude.GT = new Fay$$$(function(){
  return new Prelude._GT();
});
Prelude._LT = function LT(){
};
Prelude._LT.prototype.instance = "LT";
Prelude.LT = new Fay$$$(function(){
  return new Prelude._LT();
});
Prelude._EQ = function EQ(){
};
Prelude._EQ.prototype.instance = "EQ";
Prelude.EQ = new Fay$$$(function(){
  return new Prelude._EQ();
});
Prelude.compare = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude.GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude.LT : Prelude.EQ;
    });
  };
};
Prelude.succ = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$add)(x))(1);
  });
};
Prelude.pred = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$sub)(x))(1);
  });
};
Prelude.enumFrom = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude.enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));
  });
};
Prelude.enumFromTo = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var n = $p2;
      var i = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude.enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));
    });
  };
};
Prelude.enumFromBy = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var by = $p2;
      var fr = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude.enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));
    });
  };
};
Prelude.enumFromThen = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var th = $p2;
      var fr = $p1;
      return Fay$$_(Fay$$_(Prelude.enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));
    });
  };
};
Prelude.enumFromByTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var by = $p2;
        var fr = $p1;
        return (function(){
          var neg = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));
            });
          };
          var pos = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));
            });
          };
          return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);
        })();
      });
    };
  };
};
Prelude.enumFromThenTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var th = $p2;
        var fr = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude.enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);
      });
    };
  };
};
Prelude.fromIntegral = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.fromInteger = function($p1){
  return new Fay$$$(function(){
    return $p1;
  });
};
Prelude.not = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
Prelude.otherwise = true;
Prelude.show = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));
  });
};
Prelude.error = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
Prelude.$_undefined = new Fay$$$(function(){
  return Fay$$_(Prelude.error)(Fay$$list("Prelude.undefined"));
});
Prelude.either = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof Prelude._Left) {
          var a = Fay$$_($p3).slot1;
          var f = $p1;
          return Fay$$_(f)(a);
        }
        if (Fay$$_($p3) instanceof Prelude._Right) {
          var b = Fay$$_($p3).slot1;
          var g = $p2;
          return Fay$$_(g)(b);
        }
        throw ["unhandled case in either",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.until = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var f = $p2;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude.until)(p))(f))(Fay$$_(f)(x));
      });
    };
  };
};
Prelude.$36$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));
    });
  };
};
Prelude.$_const = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p1;
      return a;
    });
  };
};
Prelude.id = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return x;
  });
};
Prelude.$46$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$_(g)(x));
      });
    };
  };
};
Prelude.$36$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(f)(x);
    });
  };
};
Prelude.flip = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(y))(x);
      });
    };
  };
};
Prelude.curry = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$list([x,y]));
      });
    };
  };
};
Prelude.uncurry = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var f = $p1;
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1));
          var y = Fay$$index(1,Fay$$_($tmp1));
          return Fay$$_(Fay$$_(f)(x))(y);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(p);
    });
  };
};
Prelude.snd = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(1,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in snd",[$p1]];
  });
};
Prelude.fst = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(0,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in fst",[$p1]];
  });
};
Prelude.div = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {
          return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude.quot)(x))(y);
    });
  };
};
Prelude.mod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {
          return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude.rem)(x))(y);
    });
  };
};
Prelude.divMod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var q = Fay$$index(0,Fay$$_($tmp1));
            var r = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var q = Fay$$index(0,Fay$$_($tmp1));
              var r = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude.quotRem)(x))(y);
    });
  };
};
Prelude.min = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.min(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.max = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.max(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.recip = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(1))(x);
  });
};
Prelude.negate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
Prelude.abs = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.negate)(x) : x;
  });
};
Prelude.signum = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));
  });
};
Prelude.pi = new Fay$$$(function(){
  return Fay$$jsToFay_double(Math.PI);
});
Prelude.exp = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
Prelude.sqrt = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
Prelude.log = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
Prelude.$42$$42$ = new Fay$$$(function(){
  return Prelude.unsafePow;
});
Prelude.$94$$94$ = new Fay$$$(function(){
  return Prelude.unsafePow;
});
Prelude.unsafePow = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.pow(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));
    });
  };
};
Prelude.$94$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {
        return Fay$$_(Prelude.error)(Fay$$list("(^): negative exponent"));
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {
          return 1;
        } else {if (Fay$$_(Fay$$_(Prelude.even)(b))) {
            return (function(){
              return new Fay$$$(function(){
                var x = new Fay$$$(function(){
                  return Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Prelude.quot)(b))(2));
                });
                return Fay$$_(Fay$$_(Fay$$mult)(x))(x);
              });
            })();
          }
        }
      }
      var b = $p2;
      var a = $p1;
      return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));
    });
  };
};
Prelude.logBase = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var b = $p1;
      return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(x)))(Fay$$_(Prelude.log)(b));
    });
  };
};
Prelude.sin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));
  });
};
Prelude.tan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));
  });
};
Prelude.cos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));
  });
};
Prelude.asin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));
  });
};
Prelude.atan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));
  });
};
Prelude.acos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));
  });
};
Prelude.sinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);
  });
};
Prelude.tanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      return new Fay$$$(function(){
        var a = new Fay$$$(function(){
          return Fay$$_(Prelude.exp)(x);
        });
        var b = new Fay$$$(function(){
          return Fay$$_(Prelude.exp)((-(Fay$$_(x))));
        });
        return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));
      });
    })();
  });
};
Prelude.cosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);
  });
};
Prelude.asinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));
  });
};
Prelude.atanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);
  });
};
Prelude.acosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));
  });
};
Prelude.properFraction = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      return new Fay$$$(function(){
        var a = new Fay$$$(function(){
          return Fay$$_(Prelude.truncate)(x);
        });
        return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude.fromIntegral)(a))]);
      });
    })();
  });
};
Prelude.truncate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.ceiling)(x) : Fay$$_(Prelude.floor)(x);
  });
};
Prelude.round = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));
  });
};
Prelude.ceiling = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
Prelude.floor = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
Prelude.subtract = new Fay$$$(function(){
  return Fay$$_(Prelude.flip)(Fay$$sub);
});
Prelude.even = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude.rem)(x))(2)))(0);
  });
};
Prelude.odd = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude.not)(Fay$$_(Prelude.even)(x));
  });
};
Prelude.gcd = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p2) === 0) {
                var x = $p1;
                return x;
              }
              var y = $p2;
              var x = $p1;
              return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude.rem)(x))(y));
            });
          };
        };
        return Fay$$_(Fay$$_(go)(Fay$$_(Prelude.abs)(a)))(Fay$$_(Prelude.abs)(b));
      })();
    });
  };
};
Prelude.quot = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.quot$39$)(x))(y);
    });
  };
};
Prelude.quot$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));
    });
  };
};
Prelude.quotRem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$list([Fay$$_(Fay$$_(Prelude.quot)(x))(y),Fay$$_(Fay$$_(Prelude.rem)(x))(y)]);
    });
  };
};
Prelude.rem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.rem$39$)(x))(y);
    });
  };
};
Prelude.rem$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));
    });
  };
};
Prelude.lcm = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === 0) {
        return 0;
      }
      if (Fay$$_($p1) === 0) {
        return 0;
      }
      var b = $p2;
      var a = $p1;
      return Fay$$_(Prelude.abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude.quot)(a))(Fay$$_(Fay$$_(Prelude.gcd)(a))(b))))(b));
    });
  };
};
Prelude.find = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude.Just)(x) : Fay$$_(Fay$$_(Prelude.find)(p))(xs);
      }
      if (Fay$$_($p2) === null) {
        return Prelude.Nothing;
      }
      throw ["unhandled case in find",[$p1,$p2]];
    });
  };
};
Prelude.filter = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude.filter)(p))(xs);
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      throw ["unhandled case in filter",[$p1,$p2]];
    });
  };
};
Prelude.$_null = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    return false;
  });
};
Prelude.map = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude.map)(f))(xs));
      }
      throw ["unhandled case in map",[$p1,$p2]];
    });
  };
};
Prelude.nub = function($p1){
  return new Fay$$$(function(){
    var ls = $p1;
    return Fay$$_(Fay$$_(Prelude.nub$39$)(ls))(null);
  });
};
Prelude.nub$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === null) {
        return null;
      }
      var ls = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$_(Prelude.elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));
      }
      throw ["unhandled case in nub'",[$p1,$p2]];
    });
  };
};
Prelude.elem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var y = $tmp1.car;
        var ys = $tmp1.cdr;
        var x = $p1;
        return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));
      }
      if (Fay$$_($p2) === null) {
        return false;
      }
      throw ["unhandled case in elem",[$p1,$p2]];
    });
  };
};
Prelude.notElem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var x = $p1;
      return Fay$$_(Prelude.not)(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));
    });
  };
};
Prelude.sort = new Fay$$$(function(){
  return Fay$$_(Prelude.sortBy)(Prelude.compare);
});
Prelude.sortBy = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Prelude.insertBy)(cmp)))(null);
  });
};
Prelude.insertBy = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var x = $p2;
          return Fay$$list([x]);
        }
        var ys = $p3;
        var x = $p2;
        var cmp = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return Fay$$list([x]);
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var y = $tmp2.car;
            var ys$39$ = $tmp2.cdr;
            return (function($tmp2){
              if (Fay$$_($tmp2) instanceof Prelude._GT) {
                return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude.insertBy)(cmp))(x))(ys$39$));
              }
              return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);
            })(Fay$$_(Fay$$_(cmp)(x))(y));
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(ys);
      });
    };
  };
};
Prelude.conc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.conc)(xs))(ys));
      }
      var ys = $p2;
      if (Fay$$_($p1) === null) {
        return ys;
      }
      throw ["unhandled case in conc",[$p1,$p2]];
    });
  };
};
Prelude.concat = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude.foldr)(Prelude.conc))(null);
});
Prelude.concatMap = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.$43$$43$))(f)))(null);
  });
};
Prelude.foldr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(f))(z))(xs));
        }
        throw ["unhandled case in foldr",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.foldr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return x;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude.foldr1)(f))(xs));
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Prelude.error)(Fay$$list("foldr1: empty list"));
      }
      throw ["unhandled case in foldr1",[$p1,$p2]];
    });
  };
};
Prelude.foldl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);
        }
        throw ["unhandled case in foldl",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.foldl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(x))(xs);
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Prelude.error)(Fay$$list("foldl1: empty list"));
      }
      throw ["unhandled case in foldl1",[$p1,$p2]];
    });
  };
};
Prelude.$43$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude.conc)(x))(y);
    });
  };
};
Prelude.$33$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p1) === null) {
                return Fay$$_(Prelude.error)(Fay$$list("(!!): index too large"));
              }
              if (Fay$$_($p2) === 0) {
                var $tmp1 = Fay$$_($p1);
                if ($tmp1 instanceof Fay$$Cons) {
                  var h = $tmp1.car;
                  return h;
                }
              }
              var n = $p2;
              var $tmp1 = Fay$$_($p1);
              if ($tmp1 instanceof Fay$$Cons) {
                var t = $tmp1.cdr;
                return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));
              }
              throw ["unhandled case in go",[$p1,$p2]];
            });
          };
        };
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude.error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);
      })();
    });
  };
};
Prelude.head = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("head: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      return h;
    }
    throw ["unhandled case in head",[$p1]];
  });
};
Prelude.tail = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("tail: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return t;
    }
    throw ["unhandled case in tail",[$p1]];
  });
};
Prelude.init = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      var a = Fay$$index(0,Fay$$_($p1));
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude.init)(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
Prelude.last = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("last: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      var a = Fay$$index(0,Fay$$_($p1));
      return a;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return Fay$$_(Prelude.last)(t);
    }
    throw ["unhandled case in last",[$p1]];
  });
};
Prelude.iterate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.iterate)(f))(Fay$$_(f)(x)));
    });
  };
};
Prelude.repeat = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude.repeat)(x));
  });
};
Prelude.replicate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      var x = $p2;
      var n = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));
    });
  };
};
Prelude.cycle = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("cycle: empty list"));
    }
    var xs = $p1;
    return (function(){
      var xs$39$ = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Prelude.$43$$43$)(xs))(xs$39$);
      });
      return xs$39$;
    })();
  });
};
Prelude.take = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));
      }
      throw ["unhandled case in take",[$p1,$p2]];
    });
  };
};
Prelude.drop = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return xs;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var xss = $p2;
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? xss : Fay$$_(Fay$$_(Prelude.drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);
      }
      throw ["unhandled case in drop",[$p1,$p2]];
    });
  };
};
Prelude.splitAt = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return Fay$$list([null,xs]);
      }
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude.splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));
      }
      throw ["unhandled case in splitAt",[$p1,$p2]];
    });
  };
};
Prelude.takeWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.takeWhile)(p))(xs)) : null;
      }
      throw ["unhandled case in takeWhile",[$p1,$p2]];
    });
  };
};
Prelude.dropWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude.dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);
      }
      throw ["unhandled case in dropWhile",[$p1,$p2]];
    });
  };
};
Prelude.span = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude.span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);
      }
      throw ["unhandled case in span",[$p1,$p2]];
    });
  };
};
Prelude.$_break = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(Prelude.span)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.not))(p));
  });
};
Prelude.zipWith = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var b = $tmp1.car;
          var bs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var a = $tmp1.car;
            var as = $tmp1.cdr;
            var f = $p1;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith)(f))(as))(bs));
          }
        }
        return null;
      });
    };
  };
};
Prelude.zipWith3 = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p4);
          if ($tmp1 instanceof Fay$$Cons) {
            var c = $tmp1.car;
            var cs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p3);
            if ($tmp1 instanceof Fay$$Cons) {
              var b = $tmp1.car;
              var bs = $tmp1.cdr;
              var $tmp1 = Fay$$_($p2);
              if ($tmp1 instanceof Fay$$Cons) {
                var a = $tmp1.car;
                var as = $tmp1.cdr;
                var f = $p1;
                return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith3)(f))(as))(bs))(cs));
              }
            }
          }
          return null;
        });
      };
    };
  };
};
Prelude.zip = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = Fay$$_($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude.zip)(as))(bs));
        }
      }
      return null;
    });
  };
};
Prelude.zip3 = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var c = $tmp1.car;
          var cs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var b = $tmp1.car;
            var bs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p1);
            if ($tmp1 instanceof Fay$$Cons) {
              var a = $tmp1.car;
              var as = $tmp1.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude.zip3)(as))(bs))(cs));
            }
          }
        }
        return null;
      });
    };
  };
};
Prelude.unzip = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Prelude.unzip)(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
Prelude.unzip3 = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Prelude.unzip3)(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
Prelude.lines = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
        }
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.lines)(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Fay$$_(Prelude.$_break)(isLineBreak))(s));
    })();
  });
};
Prelude.unlines = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var l = $tmp1.car;
      var ls = $tmp1.cdr;
      return Fay$$_(Fay$$_(Prelude.$43$$43$)(l))(Fay$$_(Fay$$_(Fay$$cons)("\n"))(Fay$$_(Prelude.unlines)(ls)));
    }
    throw ["unhandled case in unlines",[$p1]];
  });
};
Prelude.words = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.words)(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Prelude.$_break)(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$_(Fay$$_(Prelude.elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude.dropWhile)(isSpace))(str));
    })();
  });
};
Prelude.unwords = new Fay$$$(function(){
  return Fay$$_(Prelude.intercalate)(Fay$$list(" "));
});
Prelude.and = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude.and)(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
Prelude.or = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude.or)(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
Prelude.any = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return false;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.any)(p))(xs));
      }
      throw ["unhandled case in any",[$p1,$p2]];
    });
  };
};
Prelude.all = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return true;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.all)(p))(xs));
      }
      throw ["unhandled case in all",[$p1,$p2]];
    });
  };
};
Prelude.intersperse = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs));
      }
      throw ["unhandled case in intersperse",[$p1,$p2]];
    });
  };
};
Prelude.prependToAll = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs)));
      }
      throw ["unhandled case in prependToAll",[$p1,$p2]];
    });
  };
};
Prelude.intercalate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xss = $p2;
      var xs = $p1;
      return Fay$$_(Prelude.concat)(Fay$$_(Fay$$_(Prelude.intersperse)(xs))(xss));
    });
  };
};
Prelude.maximum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("maximum: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.max))(xs);
  });
};
Prelude.minimum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude.error)(Fay$$list("minimum: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.min))(xs);
  });
};
Prelude.product = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$mult))(1))(xs);
  });
};
Prelude.sum = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$add))(0))(xs);
  });
};
Prelude.scanl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var l = $p3;
        var z = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return null;
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var x = $tmp2.car;
            var xs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(l));
      });
    };
  };
};
Prelude.scanl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(x))(xs);
      }
      throw ["unhandled case in scanl1",[$p1,$p2]];
    });
  };
};
Prelude.scanr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return Fay$$list([z]);
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return (function($tmp1){
            var $tmp2 = Fay$$_($tmp1);
            if ($tmp2 instanceof Fay$$Cons) {
              var h = $tmp2.car;
              var t = $tmp2.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
            }
            return Prelude.$_undefined;
          })(Fay$$_(Fay$$_(Fay$$_(Prelude.scanr)(f))(z))(xs));
        }
        throw ["unhandled case in scanr",[$p1,$p2,$p3]];
      });
    };
  };
};
Prelude.scanr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return Fay$$list([x]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return (function($tmp1){
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var h = $tmp2.car;
            var t = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
          }
          return Prelude.$_undefined;
        })(Fay$$_(Fay$$_(Prelude.scanr1)(f))(xs));
      }
      throw ["unhandled case in scanr1",[$p1,$p2]];
    });
  };
};
Prelude.lookup = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        var _key = $p1;
        return Prelude.Nothing;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1.car));
          var y = Fay$$index(1,Fay$$_($tmp1.car));
          var xys = $tmp1.cdr;
          var key = $p1;
          return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude.Just)(y) : Fay$$_(Fay$$_(Prelude.lookup)(key))(xys);
        }
      }
      throw ["unhandled case in lookup",[$p1,$p2]];
    });
  };
};
Prelude.length = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude.length$39$)(0))(xs);
  });
};
Prelude.length$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var acc = $p1;
        return Fay$$_(Fay$$_(Prelude.length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);
      }
      var acc = $p1;
      return acc;
    });
  };
};
Prelude.reverse = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(Prelude.reverse)(xs)))(Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
Prelude.print = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));
  });
};
Prelude.putStrLn = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));
  });
};
Prelude.ifThenElse = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var b = $p3;
        var a = $p2;
        var p = $p1;
        return Fay$$_(p) ? a : b;
      });
    };
  };
};
Fay$$objConcat(Fay$$fayToJsHash,{"Just": function(type,argTypes,_obj){
  var obj_ = {"instance": "Just"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Nothing": function(type,argTypes,_obj){
  var obj_ = {"instance": "Nothing"};
  return obj_;
},"Left": function(type,argTypes,_obj){
  var obj_ = {"instance": "Left"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Right": function(type,argTypes,_obj){
  var obj_ = {"instance": "Right"};
  var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"GT": function(type,argTypes,_obj){
  var obj_ = {"instance": "GT"};
  return obj_;
},"LT": function(type,argTypes,_obj){
  var obj_ = {"instance": "LT"};
  return obj_;
},"EQ": function(type,argTypes,_obj){
  var obj_ = {"instance": "EQ"};
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Just": function(type,argTypes,obj){
  return new Prelude._Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Nothing": function(type,argTypes,obj){
  return new Prelude._Nothing();
},"Left": function(type,argTypes,obj){
  return new Prelude._Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"Right": function(type,argTypes,obj){
  return new Prelude._Right(Fay$$jsToFay(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
},"GT": function(type,argTypes,obj){
  return new Prelude._GT();
},"LT": function(type,argTypes,obj){
  return new Prelude._LT();
},"EQ": function(type,argTypes,obj){
  return new Prelude._EQ();
}});
var FFI = {};
Fay.Text = {};
Fay.Text.Type = {};
Fay.Text.Type.pack = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs_string($p1));
  });
};
Fay.Text.Type.unpack = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(Fay$$fayToJs(["user","Text",[]],$p1));
  });
};
Fay.Text.Type.fromString = new Fay$$$(function(){
  return Fay.Text.Type.pack;
});
Fay.Text.empty = new Fay$$$(function(){
  return Fay$$jsToFay(["user","Text",[]],'');
});
Fay.Text.cons = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Char",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Fay.Text.snoc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Char",[]],$p2));
    });
  };
};
Fay.Text.append = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Fay.Text.uncons = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Maybe",[["tuple",[["user","Char",[]],["user","Text",[]]]]]],Fay$$fayToJs(["user","Text",[]],$p1)[0] ? { instance: 'Just', slot1 : [Fay$$fayToJs(["user","Text",[]],$p1)[0],Fay$$fayToJs(["user","Text",[]],$p1).slice(1)] } : { instance : 'Nothing' });
  });
};
Fay.Text.head = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1)[0] || (function () {throw new Error('Fay.Text.head: empty Text'); }()));
  });
};
Fay.Text.last = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1)[Fay$$fayToJs(["user","Text",[]],$p1).length-1] : (function() { throw new Error('Fay.Text.last: empty Text') })());
  });
};
Fay.Text.tail = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(1) : (function () { throw new Error('Fay.Text.tail: empty Text') })());
  });
};
Fay.Text.init = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).length ? Fay$$fayToJs(["user","Text",[]],$p1).slice(0,-1) : (function () { throw new Error('Fay.Text.init: empty Text') })());
  });
};
Fay.Text.$_null = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_bool(!(Fay$$fayToJs(["user","Text",[]],$p1).length));
  });
};
Fay.Text.length = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs(["user","Text",[]],$p1).length);
  });
};
Fay.Text.map = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Char",[]]]],$p1)).join(''));
    });
  };
};
Fay.Text.intercalate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p2).join(Fay$$fayToJs(["user","Text",[]],$p1)));
    });
  };
};
Fay.Text.intersperse = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p2).split('').join(Fay$$fayToJs(["user","Char",[]],$p1)));
    });
  };
};
Fay.Text.reverse = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).split('').reverse().join(''));
  });
};
Fay.Text.toLower = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toLowerCase());
  });
};
Fay.Text.toUpper = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toUpperCase());
  });
};
Fay.Text.concat = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p1).join(''));
  });
};
Fay.Text.concatMap = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],[].map.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["user","Text",[]]]],$p1)).join(''));
    });
  };
};
Fay.Text.any = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length > 0);
    });
  };
};
Fay.Text.all = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool([].filter.call(Fay$$fayToJs(["user","Text",[]],$p2), Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1)).length == Fay$$fayToJs(["function",[["user","Char",[]],["bool"]]],$p1).length);
    });
  };
};
Fay.Text.maximum = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Fay.Text.maximum: empty string'); }    var max = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] > max) { max = s[i]; }    }    return max;  })(Fay$$fayToJs(["user","Text",[]],$p1)));
  });
};
Fay.Text.minimum = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Char",[]],(function (s) {    if (s === '') { throw new Error('Fay.Text.maximum: empty string'); }    var min = s[0];    for (var i = 1; i < s.length; s++) {      if (s[i] < min) { min = s[i]; }    }    return min;  })(Fay$$fayToJs(["user","Text",[]],$p1)));
  });
};
Fay.Text.fromString = Fay.Text.Type.fromString;
Fay.Text.pack = Fay.Text.Type.pack;
Fay.Text.unpack = Fay.Text.Type.unpack;
var Pickler = {};
Pickler.Text = {};
Pickler.Text.lower = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1).toLowerCase());
  });
};
Pickler.Text.split = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["list",[["user","Text",[]]]],Fay$$fayToJs(["user","Text",[]],$p2).split(Fay$$fayToJs(["user","Text",[]],$p1)));
    });
  };
};
Pickler.Text.join = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["list",[["user","Text",[]]]],$p2).join(Fay$$fayToJs(["user","Text",[]],$p1)));
    });
  };
};
Pickler.Text.textLength = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Fay$$fayToJs(["user","Text",[]],$p1).length);
  });
};
Pickler.Text.$60$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["user","Text",[]],Fay$$fayToJs(["user","Text",[]],$p1) + Fay$$fayToJs(["user","Text",[]],$p2));
    });
  };
};
Pickler.Object = {};
Pickler.Object.emptyObj = new Fay$$$(function(){
  return Fay$$jsToFay(["user","Object",[["unknown"]]],{});
});
Pickler.Object.get = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["automatic"],Fay$$fayToJs(["user","Object",[["tuple",[["unknown"],["unknown"]]]]],$p2)[Fay$$fayToJs(["user","Text",[]],$p1)]);
    });
  };
};
Pickler.Object.has = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Object",[["unknown"]]],$p2)[Fay$$fayToJs(["user","Text",[]],$p1)] !== undefined);
    });
  };
};
Pickler.Object.set = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return Fay$$jsToFay(["user","Object",[["tuple",[["unknown"],["unknown"]]]]],(function (k, v, i)
 { var o = {};
 for (var p in i) o[p] = i[p];
 o[k] = v;
 return o; })
(Fay$$fayToJs(["user","Text",[]],$p1), Fay$$fayToJs(["automatic"],$p2), Fay$$fayToJs(["user","Object",[["unknown"]]],$p3)));
      });
    };
  };
};
Pickler.Object.cast = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["user","Object",[["unknown"]]],Fay$$fayToJs(["user","Object",[["tuple",[["unknown"],["unknown"]]]]],$p1));
  });
};
var Strict = {};
Strict.Pickler = {};
Pickler._Prim = function Prim(slot1){
  this.slot1 = slot1;
};
Pickler._Prim.prototype.instance = "Prim";
Pickler.Prim = function(slot1){
  return new Fay$$$(function(){
    return new Pickler._Prim(slot1);
  });
};
Pickler._And = function And(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
Pickler._And.prototype.instance = "And";
Pickler.And = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new Pickler._And(slot1,slot2);
    });
  };
};
Pickler._Or = function Or(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
Pickler._Or.prototype.instance = "Or";
Pickler.Or = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new Pickler._Or(slot1,slot2);
    });
  };
};
Pickler._Many = function Many(slot1){
  this.slot1 = slot1;
};
Pickler._Many.prototype.instance = "Many";
Pickler.Many = function(slot1){
  return new Fay$$$(function(){
    return new Pickler._Many(slot1);
  });
};
Pickler._Some = function Some(slot1){
  this.slot1 = slot1;
};
Pickler._Some.prototype.instance = "Some";
Pickler.Some = function(slot1){
  return new Fay$$$(function(){
    return new Pickler._Some(slot1);
  });
};
Pickler.showError = new Fay$$$(function(){
  return new Fay$$$(function(){
    var go = function($p1){
      return new Fay$$$(function(){
        var e = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Pickler._Prim) {
            var t = Fay$$_($tmp1).slot1;
            return Fay$$list([t]);
          }
          if (Fay$$_($tmp1) instanceof Pickler._And) {
            var a = Fay$$_($tmp1).slot1;
            var b = Fay$$_($tmp1).slot2;
            return Fay$$_(paren)(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(go)(a)))(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list(["and"])))(Fay$$_(go)(b))));
          }
          if (Fay$$_($tmp1) instanceof Pickler._Or) {
            var a = Fay$$_($tmp1).slot1;
            var b = Fay$$_($tmp1).slot2;
            return Fay$$_(paren)(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(go)(a)))(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list(["or"])))(Fay$$_(go)(b))));
          }
          if (Fay$$_($tmp1) instanceof Pickler._Many) {
            var a = Fay$$_($tmp1).slot1;
            return Fay$$_(paren)(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list(["many of"])))(Fay$$_(go)(a)));
          }
          if (Fay$$_($tmp1) instanceof Pickler._Some) {
            var a = Fay$$_($tmp1).slot1;
            return Fay$$_(paren)(Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list(["some of"])))(Fay$$_(go)(a)));
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(e);
      });
    };
    var paren = function($p1){
      return new Fay$$$(function(){
        var x = $p1;
        return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$list(["("])))(Fay$$_(Fay$$_(Prelude.$43$$43$)(x))(Fay$$list([")"])));
      });
    };
    return Fay$$_(Fay$$_(Prelude.$46$)(Fay$$_(Pickler.Text.join)(" ")))(go);
  });
});
Pickler._Pickler = function Pickler(_parser,_printer,_label){
  this._parser = _parser;
  this._printer = _printer;
  this._label = _label;
};
Pickler._Pickler.prototype.instance = "Pickler";
Pickler.Pickler = function(_parser){
  return function(_printer){
    return function(_label){
      return new Fay$$$(function(){
        return new Pickler._Pickler(_parser,_printer,_label);
      });
    };
  };
};
Pickler._parser = function(x){
  return new Fay$$$(function(){
    return Fay$$_(x)._parser;
  });
};
Pickler._printer = function(x){
  return new Fay$$$(function(){
    return Fay$$_(x)._printer;
  });
};
Pickler._label = function(x){
  return new Fay$$$(function(){
    return Fay$$_(x)._label;
  });
};
Pickler.parser = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new Fay$$$(function(){
          var ok = $p4;
          var err = $p3;
          var i = $p2;
          var p = $p1;
          return (function($tmp1){
            if (Fay$$_($tmp1) instanceof Prelude._Left) {
              var e = Fay$$_($tmp1).slot1;
              return Fay$$_(err)(e);
            }
            if (Fay$$_($tmp1) instanceof Prelude._Right) {
              if (Fay$$listLen(Fay$$_(Fay$$_($tmp1).slot1),2)) {
                var r = Fay$$index(0,Fay$$_(Fay$$_($tmp1).slot1));
                return Fay$$_(ok)(r);
              }
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Pickler._parser)(p))(i));
        });
      };
    };
  };
};
Pickler.printer = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new Fay$$$(function(){
          var ok = $p4;
          var err = $p3;
          var o = $p2;
          var p = $p1;
          return (function($tmp1){
            if (Fay$$_($tmp1) instanceof Prelude._Left) {
              var e = Fay$$_($tmp1).slot1;
              return Fay$$_(err)(e);
            }
            if (Fay$$_($tmp1) instanceof Prelude._Right) {
              var r = Fay$$_($tmp1).slot1;
              return Fay$$_(ok)(r);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Pickler._printer)(p))(o));
        });
      };
    };
  };
};
Pickler.parseAnd = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var xs = $p3;
        var cont = $p2;
        var p = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Prelude._Left) {
            var e = Fay$$_($tmp1).slot1;
            return Fay$$_(Prelude.Left)(e);
          }
          if (Fay$$_($tmp1) instanceof Prelude._Right) {
            if (Fay$$listLen(Fay$$_(Fay$$_($tmp1).slot1),2)) {
              var o = Fay$$index(0,Fay$$_(Fay$$_($tmp1).slot1));
              var ys = Fay$$index(1,Fay$$_(Fay$$_($tmp1).slot1));
              return Fay$$_(Fay$$_(cont)(o))(ys);
            }
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Pickler._parser)(p))(xs));
      });
    };
  };
};
Pickler.parseOr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var xs = $p3;
        var cont = $p2;
        var p = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Prelude._Left) {
            var e = Fay$$_($tmp1).slot1;
            return Fay$$_(Fay$$_(cont)(e))(xs);
          }
          if (Fay$$_($tmp1) instanceof Prelude._Right) {
            var o = Fay$$_($tmp1).slot1;
            return Fay$$_(Prelude.Right)(o);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Pickler._parser)(p))(xs));
      });
    };
  };
};
Pickler.printAnd = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var xs = $p3;
        var cont = $p2;
        var p = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Prelude._Left) {
            var e = Fay$$_($tmp1).slot1;
            return Fay$$_(Prelude.Left)(e);
          }
          if (Fay$$_($tmp1) instanceof Prelude._Right) {
            var i = Fay$$_($tmp1).slot1;
            return Fay$$_(Fay$$_(cont)(i))(xs);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Pickler._printer)(p))(xs));
      });
    };
  };
};
Pickler.printOr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var xs = $p3;
        var cont = $p2;
        var p = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) instanceof Prelude._Left) {
            var e = Fay$$_($tmp1).slot1;
            return Fay$$_(Fay$$_(cont)(e))(xs);
          }
          if (Fay$$_($tmp1) instanceof Prelude._Right) {
            var i = Fay$$_($tmp1).slot1;
            return Fay$$_(Prelude.Right)(i);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Pickler._printer)(p))(xs));
      });
    };
  };
};
Pickler.$_label = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p2;
      var l = $p1;
      return (function(){
        var p = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Pickler.parseOr)(a))(e);
        });
        var q = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Pickler.printOr)(a))(e);
        });
        var e = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              return Fay$$_(Prelude.Left)(l);
            });
          };
        };
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(l);
      })();
    });
  };
};
Pickler.$_for = new Fay$$$(function(){
  return Pickler.$62$$45$;
});
Pickler.fmap = new Fay$$$(function(){
  return Pickler.$60$$36$$62$;
});
Pickler.app = new Fay$$$(function(){
  return Pickler.$60$$42$$62$;
});
Pickler.alt = new Fay$$$(function(){
  return Pickler.$60$$124$$62$;
});
Pickler.$60$$36$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p2;
      var f = $p1;
      return (function(){
        var p = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseAnd)(a)))(function($p1){
            var o = $p1;
            return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.Right))(Fay$$_(function($p1){
              var $gen_0 = $p1;
              return function($p2){
                var $gen_1 = $p2;
                return Fay$$list([$gen_0,$gen_1]);
              };
            })(Fay$$_(f)(o)));
          });
        });
        var q = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.printAnd)(a)))(function($p1){
            var i = $p1;
            return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.$_const))(Fay$$_(Prelude.Right)(i));
          });
        });
        var w = new Fay$$$(function(){
          return Fay$$_(Pickler._label)(a);
        });
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
      })();
    });
  };
};
Pickler.bimap = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var a = $p3;
        var g = $p2;
        var f = $p1;
        return (function(){
          var p = new Fay$$$(function(){
            return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseAnd)(a)))(function($p1){
              var o = $p1;
              return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.Right))(Fay$$_(function($p1){
                var $gen_0 = $p1;
                return function($p2){
                  var $gen_1 = $p2;
                  return Fay$$list([$gen_0,$gen_1]);
                };
              })(Fay$$_(f)(o)));
            });
          });
          var q = new Fay$$$(function(){
            return Fay$$_(Fay$$_(Prelude.$46$)(Fay$$_(Fay$$_(Pickler.printAnd)(a))(function($p1){
              var i = $p1;
              return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.$_const))(Fay$$_(Prelude.Right)(i));
            })))(g);
          });
          var w = new Fay$$$(function(){
            return Fay$$_(Pickler._label)(a);
          });
          return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
        })();
      });
    };
  };
};
Pickler.$62$$45$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) instanceof Pickler._Pickler) {
        var p = Fay$$_($p2)._parser;
        var q = Fay$$_($p2)._printer;
        var w = Fay$$_($p2)._label;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(Fay$$_(Fay$$_(Prelude.$46$)(q))(f)))(w);
      }
      throw ["unhandled case in (\u003e-)",[$p1,$p2]];
    });
  };
};
Pickler.pure = function($p1){
  return new Fay$$$(function(){
    var o = $p1;
    return (function(){
      var p = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.Right))(Fay$$_(function($p1){
          var $gen_0 = $p1;
          return function($p2){
            var $gen_1 = $p2;
            return Fay$$list([$gen_0,$gen_1]);
          };
        })(o));
      });
      var q = new Fay$$$(function(){
        return Fay$$_(Prelude.$_const)(Fay$$_(Prelude.Right)(null));
      });
      var w = new Fay$$$(function(){
        return Fay$$_(Pickler.Prim)("pure");
      });
      return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
    })();
  });
};
Pickler.prints = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var i = $p2;
      var a = $p1;
      return (function(){
        var p = new Fay$$$(function(){
          return Fay$$_(Pickler._parser)(a);
        });
        var q = new Fay$$$(function(){
          return Fay$$_(Prelude.$_const)(Fay$$_(Prelude.Right)(i));
        });
        var w = new Fay$$$(function(){
          return Fay$$_(Pickler.Prim)("prints");
        });
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
      })();
    });
  };
};
Pickler.$60$$42$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var p = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseAnd)(a)))(function($p1){
            var f = $p1;
            return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseAnd)(b)))(function($p1){
              var o = $p1;
              return Fay$$_(Fay$$_(Prelude.$46$)(Prelude.Right))(Fay$$_(function($p1){
                var $gen_0 = $p1;
                return function($p2){
                  var $gen_1 = $p2;
                  return Fay$$list([$gen_0,$gen_1]);
                };
              })(Fay$$_(f)(o)));
            });
          });
        });
        var q = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.printAnd)(a)))(function($p1){
            var i = $p1;
            return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.printAnd)(b)))(function($p1){
              var j = $p1;
              return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.$_const))(Fay$$_(Prelude.Right)(Fay$$_(Fay$$_(Prelude.$43$$43$)(i))(j)));
            });
          });
        });
        var w = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Pickler.And)(Fay$$_(Pickler._label)(a)))(Fay$$_(Pickler._label)(b));
        });
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
      })();
    });
  };
};
Pickler.$60$$42$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var q = $p2;
      var p = $p1;
      return Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Prelude.$_const))(p)))(q);
    });
  };
};
Pickler.$42$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var q = $p2;
      var p = $p1;
      return Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Fay$$_(Prelude.flip)(Prelude.$_const)))(p)))(q);
    });
  };
};
Pickler.$60$$124$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var p = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseOr)(a)))(function($p1){
            var e = $p1;
            return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.parseOr)(b)))(function($p1){
              var f = $p1;
              return Fay$$_(Prelude.$_const)(Fay$$_(Prelude.Left)(Fay$$_(Fay$$_(Pickler.Or)(e))(f)));
            });
          });
        });
        var q = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.printOr)(a)))(function($p1){
            var e = $p1;
            return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.printOr)(b)))(function($p1){
              var f = $p1;
              return Fay$$_(Prelude.$_const)(Fay$$_(Prelude.Left)(Fay$$_(Fay$$_(Pickler.Or)(e))(f)));
            });
          });
        });
        var w = new Fay$$$(function(){
          return Fay$$_(Fay$$_(Pickler.Or)(Fay$$_(Pickler._label)(a)))(Fay$$_(Pickler._label)(b));
        });
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
      })();
    });
  };
};
Pickler.guards = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p2;
      var g = $p1;
      return (function(){
        var q = function($p1){
          return new Fay$$$(function(){
            var i = $p1;
            if (Fay$$_(Fay$$_(g)(i))) {
              return Fay$$_(Fay$$_(Pickler._printer)(a))(i);
            }
            return Fay$$_(Prelude.Left)(Fay$$_(Pickler._label)(a));
          });
        };
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(Fay$$_(Pickler._parser)(a)))(q))(Fay$$_(Pickler._label)(a));
      })();
    });
  };
};
Pickler.msum = new Fay$$$(function(){
  return Fay$$_(Prelude.foldl1)(Pickler.$60$$124$$62$);
});
Pickler.option = function($p1){
  return new Fay$$$(function(){
    var d = $p1;
    return function($p1){
      var $gen_0 = $p1;
      return Fay$$_(Fay$$_(Pickler.$60$$124$$62$)($gen_0))(Fay$$_(Pickler.pure)(d));
    };
  });
};
Pickler.many = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.$_label)(Fay$$_(Pickler.Many)(Fay$$_(Pickler._label)(p)))))(Fay$$_(Fay$$_(Pickler.$60$$124$$62$)(Fay$$_(Fay$$_(Pickler.guards)(Pickler.cons))(Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Fay$$cons))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.head))(p))))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.tail))(Fay$$_(Pickler.many)(p))))))(Fay$$_(Fay$$_(Pickler.guards)(Pickler.nil))(Fay$$_(Pickler.pure)(null))));
  });
};
Pickler.some = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Pickler.$_label)(Fay$$_(Pickler.Some)(Fay$$_(Pickler._label)(p)))))(Fay$$_(Fay$$_(Pickler.guards)(Pickler.cons))(Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Fay$$cons))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.head))(p))))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.tail))(Fay$$_(Pickler.many)(p)))));
  });
};
Pickler.cons = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    return true;
  });
};
Pickler.nil = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    return true;
  });
};
Pickler.tuple = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(function($p1){
        var $gen_0 = $p1;
        return function($p2){
          var $gen_1 = $p2;
          return Fay$$list([$gen_0,$gen_1]);
        };
      }))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.fst))(a))))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.snd))(b));
    });
  };
};
Pickler.withDefault = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p2;
      var g = $p1;
      return (function(){
        var q = function($p1){
          return new Fay$$$(function(){
            var i = $p1;
            if (Fay$$_(Fay$$_(g)(i))) {
              return Fay$$_(Prelude.Right)(null);
            }
            var i = $p1;
            return Fay$$_(Fay$$_(Pickler._printer)(a))(i);
          });
        };
        return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(Fay$$_(Pickler._parser)(a)))(q))(Fay$$_(Pickler._label)(a));
      })();
    });
  };
};
Pickler.any = new Fay$$$(function(){
  return new Fay$$$(function(){
    var p = function($p1){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          return Fay$$_(Prelude.Right)(Fay$$list([x,xs]));
        }
        if (Fay$$_($p1) === null) {
          return Fay$$_(Prelude.Left)(w);
        }
        throw ["unhandled case in p",[$p1]];
      });
    };
    var q = function($p1){
      return new Fay$$$(function(){
        var x = $p1;
        return Fay$$_(Prelude.Right)(Fay$$list([x]));
      });
    };
    var w = new Fay$$$(function(){
      return Fay$$_(Pickler.Prim)("anything");
    });
    return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
  });
});
Pickler.satisfy = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return (function(){
      var p = function($p1){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p1);
          if ($tmp1 instanceof Fay$$Cons) {
            var x = $tmp1.car;
            var xs = $tmp1.cdr;
            if (Fay$$_(Fay$$_(f)(x))) {
              return Fay$$_(Prelude.Right)(Fay$$list([x,xs]));
            }
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var q = function($p1){
        return new Fay$$$(function(){
          var x = $p1;
          if (Fay$$_(Fay$$_(f)(x))) {
            return Fay$$_(Prelude.Right)(Fay$$list([x]));
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var w = new Fay$$$(function(){
        return Fay$$_(Pickler.Prim)("unsatisfied");
      });
      return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
    })();
  });
};
Pickler.token = function($p1){
  return new Fay$$$(function(){
    var t = $p1;
    return (function(){
      var p = function($p1){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p1);
          if ($tmp1 instanceof Fay$$Cons) {
            var x = $tmp1.car;
            var xs = $tmp1.cdr;
            if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(t))) {
              return Fay$$_(Prelude.Right)(Fay$$list([t,xs]));
            }
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var q = function($p1){
        return new Fay$$$(function(){
          var x = $p1;
          if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(t))) {
            return Fay$$_(Prelude.Right)(Fay$$list([t]));
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var w = new Fay$$$(function(){
        return Fay$$_(Pickler.Prim)(Fay$$_(Fay$$_(Pickler.Text.$60$$62$)(Fay$$_(Fay$$_(Pickler.Text.$60$$62$)("token '"))(t)))("'"));
      });
      return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
    })();
  });
};
Pickler.tokenI = function($p1){
  return new Fay$$$(function(){
    var t = $p1;
    return (function(){
      var p = function($p1){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p1);
          if ($tmp1 instanceof Fay$$Cons) {
            var x = $tmp1.car;
            var xs = $tmp1.cdr;
            if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Pickler.Text.lower)(x)))(l))) {
              return Fay$$_(Prelude.Right)(Fay$$list([x,xs]));
            }
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var q = function($p1){
        return new Fay$$$(function(){
          var x = $p1;
          if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Pickler.Text.lower)(x)))(l))) {
            return Fay$$_(Prelude.Right)(Fay$$list([x]));
          }
          return Fay$$_(Prelude.Left)(w);
        });
      };
      var l = new Fay$$$(function(){
        return Fay$$_(Pickler.Text.lower)(t);
      });
      var w = new Fay$$$(function(){
        return Fay$$_(Pickler.Prim)(Fay$$_(Fay$$_(Pickler.Text.$60$$62$)(Fay$$_(Fay$$_(Pickler.Text.$60$$62$)("token '"))(t)))("' (case insensitive)"));
      });
      return Fay$$_(Fay$$_(Fay$$_(Pickler.Pickler)(p))(q))(w);
    })();
  });
};
Pickler.oneOf = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude.$46$)(Pickler.msum))(Fay$$_(Prelude.map)(Pickler.token));
});
Pickler.oneOfI = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude.$46$)(Pickler.msum))(Fay$$_(Prelude.map)(Pickler.tokenI));
});
Pickler.prefix = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Pickler.$42$$62$)(Fay$$_(Fay$$_(Pickler.prints)(Fay$$_(Pickler.tokenI)(t)))(Fay$$list([t]))))(p);
    });
  };
};
Pickler.prefixI = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Pickler.$42$$62$)(Fay$$_(Fay$$_(Pickler.prints)(Fay$$_(Pickler.tokenI)(t)))(Fay$$list([t]))))(p);
    });
  };
};
Pickler.sep1I = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var s = $p1;
      return Fay$$_(Fay$$_(Pickler.guards)(Pickler.cons))(Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Fay$$cons))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.head))(p))))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Prelude.tail))(Fay$$_(Pickler.many)(Fay$$_(Fay$$_(Pickler.prefixI)(s))(p)))));
    });
  };
};
Pickler.named = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var b = $p3;
        var a = $p2;
        var key = $p1;
        return Fay$$_(Fay$$_(Pickler.guards)(Fay$$_(Pickler.Object.has)(key)))(Fay$$_(Fay$$_(Pickler.$60$$42$$62$)(Fay$$_(Fay$$_(Pickler.$60$$36$$62$)(Fay$$_(Pickler.Object.set)(key)))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Fay$$_(Pickler.Object.get)(key)))(a))))(Fay$$_(Fay$$_(Pickler.$62$$45$)(Pickler.Object.cast))(b)));
      });
    };
  };
};
Pickler.obj = new Fay$$$(function(){
  return Fay$$_(Pickler.pure)(Pickler.Object.emptyObj);
});
Fay$$objConcat(Fay$$fayToJsHash,{"Prim": function(type,argTypes,_obj){
  var obj_ = {"instance": "Prim"};
  var obj_slot1 = Fay$$fayToJs(["user","Text",[]],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"And": function(type,argTypes,_obj){
  var obj_ = {"instance": "And"};
  var obj_slot1 = Fay$$fayToJs(["user","Expect",[]],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  var obj_slot2 = Fay$$fayToJs(["user","Expect",[]],_obj.slot2);
  if (undefined !== obj_slot2) {
    obj_['slot2'] = obj_slot2;
  }
  return obj_;
},"Or": function(type,argTypes,_obj){
  var obj_ = {"instance": "Or"};
  var obj_slot1 = Fay$$fayToJs(["user","Expect",[]],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  var obj_slot2 = Fay$$fayToJs(["user","Expect",[]],_obj.slot2);
  if (undefined !== obj_slot2) {
    obj_['slot2'] = obj_slot2;
  }
  return obj_;
},"Many": function(type,argTypes,_obj){
  var obj_ = {"instance": "Many"};
  var obj_slot1 = Fay$$fayToJs(["user","Expect",[]],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Some": function(type,argTypes,_obj){
  var obj_ = {"instance": "Some"};
  var obj_slot1 = Fay$$fayToJs(["user","Expect",[]],_obj.slot1);
  if (undefined !== obj_slot1) {
    obj_['slot1'] = obj_slot1;
  }
  return obj_;
},"Pickler": function(type,argTypes,_obj){
  var obj_ = {"instance": "Pickler"};
  var obj__parser = Fay$$fayToJs(["function",[["list",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],["user","Either",[["user","Expect",[]],["tuple",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],["list",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]]]]]],_obj._parser);
  if (undefined !== obj__parser) {
    obj_['_parser'] = obj__parser;
  }
  var obj__printer = Fay$$fayToJs(["function",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],["user","Either",[["user","Expect",[]],["list",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]]]],_obj._printer);
  if (undefined !== obj__printer) {
    obj_['_printer'] = obj__printer;
  }
  var obj__label = Fay$$fayToJs(["user","Expect",[]],_obj._label);
  if (undefined !== obj__label) {
    obj_['_label'] = obj__label;
  }
  return obj_;
}});
Fay$$objConcat(Fay$$jsToFayHash,{"Prim": function(type,argTypes,obj){
  return new Pickler._Prim(Fay$$jsToFay(["user","Text",[]],obj["slot1"]));
},"And": function(type,argTypes,obj){
  return new Pickler._And(Fay$$jsToFay(["user","Expect",[]],obj["slot1"]),Fay$$jsToFay(["user","Expect",[]],obj["slot2"]));
},"Or": function(type,argTypes,obj){
  return new Pickler._Or(Fay$$jsToFay(["user","Expect",[]],obj["slot1"]),Fay$$jsToFay(["user","Expect",[]],obj["slot2"]));
},"Many": function(type,argTypes,obj){
  return new Pickler._Many(Fay$$jsToFay(["user","Expect",[]],obj["slot1"]));
},"Some": function(type,argTypes,obj){
  return new Pickler._Some(Fay$$jsToFay(["user","Expect",[]],obj["slot1"]));
},"Pickler": function(type,argTypes,obj){
  return new Pickler._Pickler(Fay$$jsToFay(["function",[["list",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]],["user","Either",[["user","Expect",[]],["tuple",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],["list",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]]]]]],obj["_parser"]),Fay$$jsToFay(["function",[argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],["user","Either",[["user","Expect",[]],["list",[argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"]]]]]]],obj["_printer"]),Fay$$jsToFay(["user","Expect",[]],obj["_label"]));
}});
Strict.Pickler.alt = Fay$$fayToJs(['automatic'],Pickler.alt);
Strict.Pickler.any = Fay$$fayToJs(['automatic'],Pickler.any);
Strict.Pickler.app = Fay$$fayToJs(['automatic'],Pickler.app);
Strict.Pickler.bimap = Fay$$fayToJs(['automatic'],Pickler.bimap);
Strict.Pickler.cons = Fay$$fayToJs(['automatic'],Pickler.cons);
Strict.Pickler.fmap = Fay$$fayToJs(['automatic'],Pickler.fmap);
Strict.Pickler.$_for = Fay$$fayToJs(['automatic'],Pickler.$_for);
Strict.Pickler.guards = Fay$$fayToJs(['automatic'],Pickler.guards);
Strict.Pickler.$_label = Fay$$fayToJs(['automatic'],Pickler.$_label);
Strict.Pickler.many = Fay$$fayToJs(['automatic'],Pickler.many);
Strict.Pickler.msum = Fay$$fayToJs(['automatic'],Pickler.msum);
Strict.Pickler.named = Fay$$fayToJs(['automatic'],Pickler.named);
Strict.Pickler.nil = Fay$$fayToJs(['automatic'],Pickler.nil);
Strict.Pickler.obj = Fay$$fayToJs(['automatic'],Pickler.obj);
Strict.Pickler.oneOf = Fay$$fayToJs(['automatic'],Pickler.oneOf);
Strict.Pickler.oneOfI = Fay$$fayToJs(['automatic'],Pickler.oneOfI);
Strict.Pickler.option = Fay$$fayToJs(['automatic'],Pickler.option);
Strict.Pickler.parseAnd = Fay$$fayToJs(['automatic'],Pickler.parseAnd);
Strict.Pickler.parseOr = Fay$$fayToJs(['automatic'],Pickler.parseOr);
Strict.Pickler.parser = Fay$$fayToJs(['automatic'],Pickler.parser);
Strict.Pickler.prefix = Fay$$fayToJs(['automatic'],Pickler.prefix);
Strict.Pickler.prefixI = Fay$$fayToJs(['automatic'],Pickler.prefixI);
Strict.Pickler.printAnd = Fay$$fayToJs(['automatic'],Pickler.printAnd);
Strict.Pickler.printOr = Fay$$fayToJs(['automatic'],Pickler.printOr);
Strict.Pickler.printer = Fay$$fayToJs(['automatic'],Pickler.printer);
Strict.Pickler.prints = Fay$$fayToJs(['automatic'],Pickler.prints);
Strict.Pickler.pure = Fay$$fayToJs(['automatic'],Pickler.pure);
Strict.Pickler.satisfy = Fay$$fayToJs(['automatic'],Pickler.satisfy);
Strict.Pickler.sep1I = Fay$$fayToJs(['automatic'],Pickler.sep1I);
Strict.Pickler.showError = Fay$$fayToJs(['automatic'],Pickler.showError);
Strict.Pickler.some = Fay$$fayToJs(['automatic'],Pickler.some);
Strict.Pickler.token = Fay$$fayToJs(['automatic'],Pickler.token);
Strict.Pickler.tokenI = Fay$$fayToJs(['automatic'],Pickler.tokenI);
Strict.Pickler.tuple = Fay$$fayToJs(['automatic'],Pickler.tuple);
Strict.Pickler.withDefault = Fay$$fayToJs(['automatic'],Pickler.withDefault);
Strict.Pickler.$42$$62$ = Fay$$fayToJs(['automatic'],Pickler.$42$$62$);
Strict.Pickler.$60$$36$$62$ = Fay$$fayToJs(['automatic'],Pickler.$60$$36$$62$);
Strict.Pickler.$60$$42$ = Fay$$fayToJs(['automatic'],Pickler.$60$$42$);
Strict.Pickler.$60$$42$$62$ = Fay$$fayToJs(['automatic'],Pickler.$60$$42$$62$);
Strict.Pickler.$60$$124$$62$ = Fay$$fayToJs(['automatic'],Pickler.$60$$124$$62$);
Strict.Pickler.$62$$45$ = Fay$$fayToJs(['automatic'],Pickler.$62$$45$);

