/**
 * Copyright (C) 2011 Alexander Azarov <azarov@osinka.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.osinka.subset

/** == Update Operations ==
  * Update operators and their composition. See
  * [[com.osinka.subset.update.Update]] and [[com.osinka.subset.update.Modifications]]
  * 
  * == Subset ==
  * [[com.osinka.subset.Subset]]'s `updateMatch` method lets create a "positional" update
  * modifier relative to the enclosing sub-document.
  * 
  * {{{
  * collection.update(BlogPost.comments.where{_.by === "joe"}, BlogPost.comments.updateMatch {_.votes inc 1})
  * }}}
  * 
  * This is equivalent to MongoDB shell
  * {{{
  * db.collection.update({"comments.by": "joe"}, {\$inc: {"comments.$.votes": 1}})
  * }}}
  * 
  * @see [[http://www.mongodb.org/display/DOCS/Updating#Updating-The%24positionaloperator The $ positional operator]],
  *      [[https://github.com/osinka/subset/blob/master/src/it/scala/blogCommentSpec.scala Blog Comment Example]]
  */
package object update
