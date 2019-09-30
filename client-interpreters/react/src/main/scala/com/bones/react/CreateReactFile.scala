package com.bones.react

import com.bones.data.Value.BonesSchema
import com.bones.react.FormInterpreter._
import com.bones.react.ReactDataInterpreter.{KeyHierarchy, ReactComponentData}

/**
  * Responsible for creating the React Components.
  */
object CreateReactFile {

  case class ReactFile(crudClassName: String, labelId: String, contents: String)

  def convert(bonesSchema: BonesSchema[_]): ReactFile = {

    val components = FormInterpreter.createComponents(bonesSchema)

    val className = bonesSchema.manifestOfA.runtimeClass.getSimpleName
    val entityName = Character.toLowerCase(className.charAt(0)) + className.substring(1)

    val reactData = ReactDataInterpreter.fromSchema(bonesSchema)
    val flattenedKeys = flattenKeys(reactData)

    val editComponent = components.map(component => subcomponentEdit(component.label, component.formValues, className + "CrudManager")).mkString("\n")

    val contents =
      constructor(bonesSchema, entityName, className) +
      entityDislayHeader(flattenedKeys, bonesSchema, className) +
      entityDisplayBody(entityName, className, flattenedKeys) +
      editComponent

    ReactFile(className + "CrudManager", entityName, contents)

  }

  private def flattenKeyHierarchy(keyHierarchy: KeyHierarchy) : List[String] = {
    if (keyHierarchy.children.isEmpty) List(keyHierarchy.key)
    else keyHierarchy.children.flatMap(flattenKeyHierarchy)
  }

  private def flattenKeys(componentData: List[ReactComponentData]): List[String] =
    componentData.flatMap(_.keyValues).flatMap(flattenKeyHierarchy)

  def constructor(schema: BonesSchema[_], entityName: String, className: String): String = {

    val reactData = ReactDataInterpreter.fromSchema(schema)

    val crudManagerName = className + "CrudManager"
    val realTypes = "{" + reactData.map(_.realTypes).mkString(",") + "}"
    val defaultState = "{" + reactData.map(_.defaultState).mkString(",") + "}"

    s"""
       |class ${crudManagerName} extends React.Component {
       |
       |  static baseUrl = "http://localhost:8080/${entityName}"
       |
       |  constructor(props) {
       |    super(props)
       |    this.loadEntities = this.loadEntities.bind(this);
       |    this.updateChildProperty = this.updateChildProperty.bind(this);
       |    this.handleSubmit = this.handleSubmit.bind(this);
       |    this.deleteEntity = this.deleteEntity.bind(this);
       |    this.resetValues = this.resetValues.bind(this);
       |    this.loadEntity = this.loadEntity.bind(this);
       |    this.state = {
       |      ${entityName}s: [],
       |      ${entityName}: ${crudManagerName}.defaultState()
       |    }
       |    this.loadEntities()
       |  }
       |
       |  resetValues() {
       |    this.setState({${entityName}: ${crudManagerName}.defaultState()})
       |  }
       |
       |  loadEntities() {
       |    fetch(${crudManagerName}.baseUrl, {
       |      method: "GET", // *GET, POST, PUT, DELETE, etc.
       |      mode: 'cors',
       |      headers: {
       |        Accept: 'application/json',
       |        'Content-Type': 'application/json'
       |      },
       |      credentials: 'same-origin'
       |    }).then(function(response) {
       |      return response.json();
       |    }).then(resp => {
       |      this.setState({${entityName}s: resp})
       |    }).catch(err => console.log(JSON.stringify(err)))
       |  }
       |
       |  deleteEntity(id) {
       |    fetch(${crudManagerName}.baseUrl + "/" + id, {
       |      method: "DELETE", // *GET, POST, PUT, DELETE, etc.
       |      mode: 'cors',
       |      headers: {
       |        Accept: 'application/json',
       |        'Content-Type': 'application/json'
       |      },
       |      credentials: 'same-origin'
       |    }).then(response => {
       |      this.loadEntities()
       |      console.log(response);
       |    })
       |  }
       |
       |  loadEntity(id) {
       |    fetch(${crudManagerName}.baseUrl + "/" + id, {
       |      method: "GET", // *GET, POST, PUT, DELETE, etc.
       |      mode: 'cors',
       |      headers: {
       |        Accept: 'application/json',
       |        'Content-Type': 'application/json'
       |      },
       |      credentials: 'same-origin'
       |    }).then(response => { return response.json() })
       |    .then(response => {
       |        console.log("loaded " + JSON.stringify(response));
       |        const reactified = ${crudManagerName}.addEmpty(response, ${crudManagerName}.realTypes, ${crudManagerName}.defaultState())
       |        reactified['id'] = id;
       |        console.log("reactified " + JSON.stringify(reactified));
       |        this.setState({${entityName}: reactified});
       |
       |    })
       |  }
       |
       |  static clone(obj) {
       |    var copy;
       |
       |    // Handle the 3 simple types, and null or undefined
       |    if (null == obj || "object" != typeof obj) return obj;
       |
       |    // Handle Date
       |    if (obj instanceof Date) {
       |      copy = new Date();
       |      copy.setTime(obj.getTime());
       |      return copy;
       |    }
       |
       |    // Handle Array
       |    if (obj instanceof Array) {
       |      copy = [];
       |      for (var i = 0, len = obj.length; i < len; i++) {
       |        copy[i] = ${crudManagerName}.clone(obj[i]);
       |      }
       |      return copy;
       |    }
       |
       |    // Handle Object
       |    if (obj instanceof Object) {
       |      copy = {};
       |      for (var attr in obj) {
       |        if (obj.hasOwnProperty(attr)) copy[attr] = ${crudManagerName}.clone(obj[attr]);
       |      }
       |      return copy;
       |    }
       |
       |    throw new Error("Unable to copy obj! Its type isn't supported.");
       |  }
       |
       |
       |  updateChildProperty(name) {
       |    return (key, value) => {
       |      const current = ${crudManagerName}.clone(this.state[name]);
       |      current[key] = value;
       |      this.setState({[name]:current})
       |    }
       |  }
       |
       |  handleSubmit(event) {
       |    event.preventDefault()
       |
       |    console.log("Submit state:" + JSON.stringify(this.state.${entityName}));
       |    const prunedState = ${crudManagerName}.removeEmpty(this.state.${entityName}, ${crudManagerName}.realTypes);
       |    console.log("Pruned state: " + JSON.stringify(prunedState));
       |
       |    if (typeof this.state.${entityName}.id  !== 'undefined') {
       |      fetch(${crudManagerName}.baseUrl + "/" + this.state.${entityName}.id, {
       |        method: "PUT", // *GET, POST, PUT, DELETE, etc.
       |        mode: 'cors',
       |        headers: {
       |          Accept: 'application/json',
       |          'Content-Type': 'application/json'
       |        },
       |        credentials: 'same-origin',
       |        body: JSON.stringify(prunedState)
       |      }).then(resp =>
       |        resp.json()
       |      ).then(response => {
       |        if (response.success === "false") {
       |          console.log("Failed: " + JSON.stringify(response));
       |        } else {
       |          this.setState({${entityName}:${crudManagerName}.defaultState()})
       |          this.loadEntities();
       |        }
       |      }).catch(err => console.log(JSON.stringify(err)))
       |    } else {
       |      fetch(${crudManagerName}.baseUrl, {
       |        method: "POST", // *GET, POST, PUT, DELETE, etc.
       |        mode: 'cors',
       |        headers: {
       |          Accept: 'application/json',
       |          'Content-Type': 'application/json'
       |        },
       |        credentials: 'same-origin',
       |        body: JSON.stringify(prunedState)
       |      }).then(resp => resp.json())
       |        .then(response => {
       |        if (response.success === "false") {
       |          console.log("Failed: " + JSON.stringify(response));
       |        } else {
       |          this.setState({${entityName}:${crudManagerName}.defaultState()})
       |          this.loadEntities();
       |        }
       |      }).catch(err => console.log(JSON.stringify(err)))
       |    }
       |
       |  }
       |
       |  render() {
       |    return (
       |      <div>
       |        <div className="ui container">
       |          { this.state.${entityName}Id !== null ?
       |            <div>Currently Editing Id: {this.state.${entityName}.id}</div> :
       |            <div>Creating New ${entityName.capitalize}</div>}
       |          <form onSubmit={this.handleSubmit}>
       |            <${entityName.capitalize}Edit ${entityName}={this.state.${entityName}} onUpdate={this.updateChildProperty('${entityName}')} />
       |            <input type="submit" value="Submit" className="ui primary button" />
       |            <input type="button" value="Reset" onClick={this.resetValues} className="ui button"/>
       |          </form>
       |        </div>
       |        <div className="ui container">
       |          <table className="ui celled table">
       |          <${className}Header/>
       |          {this.state.${entityName}s.map( (entity) => {
       |            return (
       |              <${entityName.capitalize}Display key={entity.id} ${entityName}={entity} onDelete={this.deleteEntity} onEdit={this.loadEntity}></${entityName.capitalize}Display>
       |            )
       |          })}
       |          </table>
       |        </div>
       |      </div>
       |    )
       |  }
       |
       |  static waterVolumeOptions = ["Low", "Average", "high"];
       |
       |  static realTypes = ${realTypes};
       |
       |  static defaultState() { return ${defaultState}; };
       |
       |  static addEmpty(obj, realTypes, defaults) {
       |    return Object.keys(realTypes)
       |      .reduce((newObj, typeName) => {
       |        const value = obj[typeName]
       |        if (typeof realTypes[typeName] === 'object') {
       |          const currentChild = value === null? {} : value;
       |          const children = ${crudManagerName}.addEmpty(currentChild, realTypes[typeName], defaults[typeName])
       |          newObj[typeName] = children;
       |        } else if (realTypes[typeName] === 'boolean') {
       |          if (value === null || typeof value === 'undefined') {
       |            newObj[typeName] = defaults[typeName];
       |          } else {
       |            newObj[typeName] = value;
       |          }
       |        } else if (realTypes[typeName] === 'date') {
       |          if (value === null || typeof value === 'undefined') {
       |            newObj[typeName] = defaults[typeName];
       |          } else {
       |            newObj[typeName] = new Date(value.substring(value, value.length - 6));
       |          }
       |        } else {
       |          if (value === null || typeof value === 'undefined') {
       |            newObj[typeName] = defaults[typeName];
       |          } else {
       |            newObj[typeName] = value.toString();
       |          }
       |        }
       |        return newObj;
       |      },{})
       |  }
       |
       |  static removeEmpty(obj, realTypes) {
       |    return Object.keys(obj)
       |      .filter(k => obj[k] !== null && obj[k] !== undefined && (typeof obj[k] !== 'string' || obj[k].trim().length > 0) && k !== 'id')  // Remove undef. and null.
       |      .reduce((newObj, k) => {
       |        if (obj[k] instanceof Date) {
       |          Object.assign(newObj, {[k]: obj[k].toISOString()})
       |        } else if (typeof obj[k] === 'object') {
       |          const reducedObj = ${crudManagerName}.removeEmpty(obj[k], realTypes[k]) // Recurse.
       |          if (Object.keys(reducedObj).length !== 0) {
       |            Object.assign(newObj, {[k]: reducedObj})
       |          }
       |        } else {
       |          const newValue = ${crudManagerName}.valueToRealType(obj[k],realTypes[k]);
       |          if (newValue !== null) {
       |            Object.assign(newObj, {[k]: newValue})  // Copy value.
       |          }
       |        }
       |        return newObj;
       |      }, {});
       |  }
       |
       |  static valueToRealType(value, realType) {
       |    console.log("value: " + value)
       |    if (value.toString().trim().length === 0) {
       |      return null;
       |    } else if (realType === 'integer') {
       |      return parseInt(value);
       |    } else if (realType === 'float') {
       |      return parseFloat(value);
       |    } else if (realType === 'boolean') {
       |      return value;
       |    } else {
       |      return value.trim();
       |    }
       |  }
       |
       |}
       |
     """.stripMargin
  }


  def entityDislayHeader(flattenKeys: List[String], schema: BonesSchema[_], className: String) = {
    s"""
       |function ${className}Header(props) {
       |
       |  return (
       |    <thead>
       |      <tr>
       |        <th></th>
       |        <th></th>
       |        ${flattenKeys.mkString("<th>","</th><th>","</th>")}
       |      </tr>
       |    </thead>
       |  )
       |}
     """.stripMargin

  }

  def entityDisplayBody(entityName: String, className: String, flattenKeys: List[String]) : String = {

    s"""
       |class ${className}Display extends React.Component {
       |
       |  render() {
       |    return (
       |      <tbody>
       |        <tr>
       |          <td><button onClick={() => this.props.onDelete(this.props.${entityName}.id)} className="ui icon button">
       |            <i className="trash icon"></i></button></td>
       |          <td><button onClick={() => this.props.onEdit(this.props.${entityName}.id)} className="ui icon button">
       |            <i className="edit icon"></i>
       |          </button></td>
       |          ${flattenKeys.map(key => s"<td>{this.props.${entityName}.${key}}</td>").mkString("")}
       |        </tr>
       |      </tbody>
       |    )
       |  }
       |}
       |
     """.stripMargin
    //           //{this.props.${entityName}.wantToVisit ? "true":"false"}</td-->
  }

  def toDisplay(formValue: ReactFormValue, parentName: String, className: String, crudManagerName: String): String = {
    formValue.inputType match {
      case StringInput(maxLength) =>
        s"""
           |        <label className="${formValue.label}">
           |          ${formValue.inputName}
           |          <input type="text" name="${formValue.label}" className="ui input"
           |                 value={this.props.${parentName}.${formValue.label}}
           |                 maxLength="${maxLength}"
           |                 onChange={this.handleChange}/>
           |        </label>
         """.stripMargin
      case SelectInput(options) =>
        s"""
           |        <label>${formValue.inputName}
           |          <select name="${formValue.label}" value={this.props.${parentName}.${formValue.label}} onChange={this.handleChange}>
           |            {
           |              ${crudManagerName}.${formValue.label}Options.map( (o,i) => {
           |                return <option key={i} value={o}>{o}</option>
           |              })
           |            }
           |          </select>
           |        </label>
           |""".stripMargin
      case Checkbox =>
        s"""
           |        <label>Want To Visit
           |          <input type="checkbox" name="${formValue.label}" checked={this.props.${parentName}.${formValue.label}}
           |                    className="ui input" onChange={this.handleChange} />
           |
           |        </label>
           |         """.stripMargin
      case LongInput(maxLength) =>
        s"""
           |        <label className="${formValue.label}">
           |          ${formValue.inputName}
           |          <input type="number" name="${formValue.label}" className="ui input"
           |                 value={this.props.${parentName}.${formValue.label}}
           |                 maxLength="${maxLength}"
           |                 onChange={this.handleChange}/>
           |        </label>
           |""".stripMargin
      case BigDecimalInput =>
        s"""
           |        <label className="${formValue.label}">
           |          ${formValue.inputName}
           |          <input type="number" name="${formValue.label}" className="ui input"
           |                 value={this.props.${parentName}.${formValue.label}}
           |                 onChange={this.handleChange}/>
           |        </label>
           |""".stripMargin
      case DateInput =>
        s"""
           |        <label>${formValue.inputName}
           |          <DateTimePicker
           |            name="${formValue.label}DatePicker"
           |            onChange={(date) => this.updateDate('${formValue.label}', date)}
           |            value={this.props.${parentName}.${formValue.label}}
           |          />
           |        </label>
           |""".stripMargin
      case TextArea(maxLength) =>
        s"""
           |<label>${formValue.inputName}
           |<textarea maxLength="${maxLength}" name="${formValue.label}" value={this.props.${parentName}.${formValue.label}}>
           |</textarea>
           |</label>
         """.stripMargin
      case ReactComponentReference(childLabel) =>
        s"""
           |          <${childLabel.capitalize}Edit
           |            ${childLabel}={this.props.${parentName}.${formValue.label}}
           |            onUpdate={this.updateChildProperty('${formValue.label}')}/>
           |""".stripMargin
        "" //TODO
    }
  }

  def subcomponentEdit(label: String, formValues: List[ReactFormValue], crudManagerName: String) = {
    val className = label.capitalize
    s"""
       |class ${className}Edit extends React.Component {
       |
       |  constructor(props) {
       |    super(props)
       |    this.handleChange = this.handleChange.bind(this);
       |    this.updateChildProperty = this.updateChildProperty.bind(this);
       |    this.updateDate = this.updateDate.bind(this);
       |  }
       |
       |  updateChildProperty(name) {
       |    return (key, value) => {
       |      const current = ${crudManagerName}.clone(this.state[name]);
       |      current[key] = value;
       |      this.props.onUpdate(name, current)
       |    }
       |  }
       |
       |  handleChange(event) {
       |    // console.log(event);
       |    // console.log(JSON.stringify(event))
       |
       |    const target = event.target
       |    const value = target.type === 'checkbox' ? target.checked : target.value
       |    const name = target.name
       |    this.props.onUpdate(name, value)
       |  }
       |
       |  updateDate(key, date) {
       |    console.log("update date. key" + key + " d:" + date);
       |    this.props.onUpdate(key, date);
       |  }
       |
       |  render() {
       |    return (
       |      <div>
       |        ${formValues.map(fv => toDisplay(fv, label, className, crudManagerName)).mkString("\n")}
       |      </div>
       |    )
       |  }
       |}
       |
     """.stripMargin
  }


}
