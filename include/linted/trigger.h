/*
 * Copyright 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LINTED_TRIGGER_H
#define LINTED_TRIGGER_H

struct linted_trigger {
	int _triggered;
};

void linted_trigger_create(struct linted_trigger *trigger);
void linted_trigger_destroy(struct linted_trigger *trigger);

void linted_trigger_set(struct linted_trigger *trigger);
void linted_trigger_wait(struct linted_trigger *trigger);

#endif /* LINTED_TRIGGER_H */
